{-# LANGUAGE OverloadedStrings #-}
import Data.FileStore hiding (create)
import Prelude hiding (id)
import Hakyll
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!),toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mconcat, mappend,(<>))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.FilePath (combine, dropExtension, takeFileName, takeDirectory, addExtension)
import qualified Data.Map as M
import Control.Applicative ((<$>))
import Control.Monad(liftM)
import System.Process(readProcessWithExitCode)
import Data.List.Split

fileStore :: FileStore
fileStore = gitFileStore "articles"

getRevisions :: FilePath -> IO [(Revision, Revision)]
getRevisions f = do
    revList <- history fileStore [f] (TimeRange Nothing Nothing) Nothing
    return $ makePairList revList
    -- eg. makePairList [1..4] == [(1,2),(2,3),(3,4)]
    where makePairList xs = [(y, x) | x <- xs, y <- xs, y == getListPrev x (reverse xs), x /= y]

getListPrev :: Eq a => a -> [a] -> a
getListPrev i l = l !! checkBounds (fromMaybe 0 (i `elemIndex` l) +1 )
    where checkBounds x | x > length l -1 = length l-1 | otherwise = x

routePage :: Routes
routePage = customRoute fileToDirectory

fileToDirectory :: Identifier -> FilePath
fileToDirectory = flip combine "index.html" . dropExtension . toFilePath

-- although it's a list of Maps, there's really only one identifier that's going to match
addRevisionList :: M.Map Identifier [(Revision, Revision)] -> Compiler String
addRevisionList d =  do
  i <- getUnderlying
  return (r (lst i))
  where lst i = M.lookup (f i) d
	f k = fromFilePath (takeFileName (toFilePath k))
	r Nothing = []
	r (Just a) = concatMap renderRevision a

-- used to create a link to the diff between two revisions
renderRevision :: (Revision, Revision) -> String
renderRevision rl = renderHtml $ H.tr $ do
    H.td $ H.toHtml $ createLink rl
    H.td $ H.toHtml $ revDescription $ fst rl
    where diffLink (a,b) = revId a ++ "_" ++ revId b ++ ".html"
          createLink (a,b) = H.a ! A.href (toValue $ diffLink (a,b)) $ H.toHtml (show (revDateTime a))

compressorCompiler :: String -> Item String -> Compiler (Item String)
compressorCompiler t = withItemBody(unixFilter "yui-compressor" ["--type", t])

-- turn a TmpFile back into a FilePath
tmpToFilePath :: TmpFile -> FilePath
tmpToFilePath (TmpFile f) = f

-- complicated. dwdiff will accept unified diff as input though.
-- options taken from http://www.gwern.net/docs/2002-radiance#diff
-- not perfect but then, not that bad either
diffCompiler :: FilePath -> (String, String) -> TmpFile -> TmpFile -> Compiler (Item String)
diffCompiler f (a,b) fnA fnB = unsafeCompiler rundiff >>= makeItem
  where rundiff = do
		    verA <- retrieve fileStore f (Just a)
		    verB <- retrieve fileStore f (Just b)
		    writeFile (tmpToFilePath fnA) verA
		    writeFile (tmpToFilePath fnB) verB
		    -- unixFilter only accepts one input.
		    -- to be even more irritating, wdiff returns an exit code of 1 for changes, and the statistics go to stderr
		    -- (ExitCode, String, String)
		    (_,o,e) <- readProcessWithExitCode "dwdiff" ["--color","--statistics","--ignore-case","--ignore-formatting","--punctuation", "--match-context=3", "--algorithm=best", tmpToFilePath fnA, tmpToFilePath fnB] []
		    (_,h,_) <- readProcessWithExitCode "aha" ["-n"] (o ++ e)
		    return h

main :: IO ()
main = hakyll $ do
    let static = route idRoute >> compile copyFileCompiler
    mapM_ (`match` static ) ["files/**", "js/lib/*", "images/**"]
    match "js/*.js" $ do
        route idRoute
        compile $ getResourceString
            >>= compressorCompiler "js"
        
    match "js/*.coffee" $ do
        route $ setExtension "js"
        compile $ getResourceString
            >>= withItemBody(unixFilter "coffee" ["--compile", "-s"])
            >>= compressorCompiler "js"
            
    match "css/*.css" $ do
        route   idRoute
        compile $ getResourceString >>= compressorCompiler "css"
        
    match "css/*.scss" $ do
        route   idRoute
        compile $ getResourceString
            >>= withItemBody(unixFilter "sass" ["-s", "--scss"])
            >>= compressorCompiler "css"
            
    match "about/*" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" commonContext
            >>= relativizeUrls
            
    tags <- buildTags "articles/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title `mappend`
                            constField "posts" list `mappend`
                            commonContext)
                >>= loadAndApplyTemplate "templates/default.html" commonContext
                >>= relativizeUrls


    -- it's a right pain in the arse that you can't use the match function here. would be nice to do something like
    -- match "articles" $ preprocess $ do ids <- getMatches
    -- or something like that.
    -- diffs ends up being Rules Map Identifier [(Revision,Revision)]
    diffs <- preprocess $ do
      -- this is a bit brittle. use Hakyll internal function to get the list of all files in here
      -- which we know is where our git repo is
      ids <- getRecursiveContents (const $ return False) "articles"
      let ids' = map fromFilePath ids
      liftM M.unions (mapM buildDiffs ids')

    -- Render articles
    _ <- ($) match "articles/*" $ do
        route routePage
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (field "history" (const (addRevisionList diffs)) <> contentCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" commonContext
	    >>= relativizeUrls

    _ <-  create (createDiffIdentifiers diffs) $ do
	route $ setExtension ".html"
	compile $ do
	  path <- toFilePath <$> getUnderlying
	  -- hacky as fuck. at this point we're operating on the created identifer
	  -- articles/articlename_reva_revb.html
	  -- but we don't have access to that element of the map of diffs.
	  -- do some nasty string splitting to get that data back
	  let revs = splitOn "_" $ dropExtension $ takeFileName path
	  let fp = flip addExtension "markdown" $  takeFileName $ takeDirectory path
	  fnA <- newTmpFile "reva"
	  fnB <- newTmpFile "revb"
	  let rs = ((head . tail) revs, head revs)
	  liftM renderPandoc (diffCompiler fp rs fnA fnB >>= applyAsTemplate (contentCtx tags))
	    >>= loadAndApplyTemplate (fromFilePath "templates/default.html") (
                    constField "author" "Andy Irving"
                    <> constField "title" "Andy Irving"
                    <> constField "description" "The personal website of Andy Irving"
                    <> commonContext) 
	    >>= relativizeUrls
    
    match "templates/*" $ compile templateCompiler
    
    create ["index.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags ("articles/*" .&&. hasNoVersion) $ fmap (take 5) . recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" (field "tagcloud" (\_ -> renderTagList tags) <> constField "title" "Andy Irving" <> constField "posts" list <> commonContext)
                >>= loadAndApplyTemplate "templates/default.html" (
                    constField "author" "Andy Irving"
                    <> constField "title" "Andy Irving"
                    <> constField "description" "The personal website of Andy Irving"
                    <> commonContext)
                >>= relativizeUrls

buildDiffs :: Identifier -> IO (M.Map Identifier [(Revision, Revision)])
buildDiffs id' = do
  revs <- getRevisions $ toFilePath id'
  return $ M.fromList [(id',revs)]
 
createDiffIdentifiers :: M.Map Identifier [(Revision, Revision)] -> [Identifier]
createDiffIdentifiers diffs = map fromFilePath (fp diffs) 
  where
    f i (a,b) = i ++ "/" ++ revId a ++ "_" ++ revId b ++ ".markdown"
    fp = M.foldrWithKey (\k x ks-> ks ++  map (f (combine "articles" $ dropExtension (toFilePath k))) x) []
  
commonContext :: Context String
commonContext = mconcat
    [ dateField "date" "%B %e, %Y"
    , field "author" $ \item -> do
        metadata <- getMetadataField (itemIdentifier item) "author" 
        return $ fromMaybe "Anonymous" metadata
    , field "description" $ \item -> do
        metadata <- getMetadataField (itemIdentifier item) "description"
        return $ fromMaybe "" metadata
    , defaultContext
    ]

contentCtx :: Tags -> Context String
contentCtx tags = mconcat
    [ tagsField "tags" tags
    , commonContext
    ]

postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (contentCtx tags) posts

