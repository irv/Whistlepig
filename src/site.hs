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
import System.FilePath (combine, dropExtension, takeFileName)
import qualified Data.Map as M
import Control.Applicative ((<$>))
import Data.Algorithm.DiffOutput
import Control.Monad(forM_)

fileStore :: FileStore
fileStore = gitFileStore "articles"

getRevisionList :: Compiler [(Revision, Revision)]
getRevisionList = do
    path <- toFilePath <$> getUnderlying
    unsafeCompiler $ getRevisions $ takeFileName path

getDiff :: FilePath -> [(Revision, Revision)] -> IO [[Diff [String]]]
getDiff page =  mapM ( getFileDiff (takeFileName page) )

--renderDiff :: [Diff, [String]] -> String
--renderDiff :: a -> String
renderDiff d = return (ppDiff d)
-- renderDiff l =  diffInd (fst l) ++ unlines (snd l) ++ diffInd (fst l) ++ nl (fst l) --renderHtml $ H.pre ! A.class_ (attrCls (fst l)) $ diffCnt l
--     where attrCls c   = toValue $ diffStr "diff" c
--           diffCnt l'   = H.toHtml $ diffInd (fst l') ++ unlines (snd l')
--           diffStr a b = a ++ show b
--           diffInd i   = case i of
--                            First -> "~~" -- strikeout
--                            Second -> "**" -- emphasis
--                            Both -> ""
--           nl i        = case i of
--                              First -> "\n"
--                              Second -> "\n"
--                              otherwise -> ""

getFileDiff :: FilePath -> (Revision, Revision) -> IO [Diff [String]]
getFileDiff f (a,b) = diff fileStore f (Just $ revId b) (Just $ revId a)

getRevisions :: FilePath -> IO [(Revision, Revision)]
getRevisions f = do
    revList <- history fileStore [f] (TimeRange Nothing Nothing) Nothing
    return $ makePairList revList
    -- eg. makePairList [1..4] == [(1,2),(2,3),(3,4)]
    where makePairList xs = [(y, x) | x <- xs, y <- xs, y == getListPrev x (reverse xs), x /= y]

getListPrev :: Eq a => a -> [a] -> a
getListPrev i l = l !! checkBounds (fromMaybe 0 (i `elemIndex` l) +1 )
    where checkBounds x | x > length l -1 = length l-1 | otherwise = x

constructDiff :: String -> [[Diff [String]]] -> Compiler (Item String)
constructDiff i d = makeItem i
    >>= loadAndApplyTemplate "templates/diff.html" (field "diff" (\_ -> diff' d) <> commonContext)
    >>= loadAndApplyTemplate "templates/default.html" commonContext
    >>= relativizeUrls
    where diff' [] =  return []
          diff' x = renderDiff $ head x

routePage :: Routes
routePage = customRoute fileToDirectory

fileToDirectory :: Identifier -> FilePath
fileToDirectory = flip combine "index.html" . dropExtension . toFilePath

addRevisionList :: Compiler String
addRevisionList = do
    path <- toFilePath <$> getUnderlying
    lst <- unsafeCompiler $ getRevisions $ takeFileName path
    return (concatMap renderRevision lst)

-- used to create a link to the diff between two revisions
renderRevision :: (Revision, Revision) -> String
renderRevision rl = renderHtml $ H.tr $ do
    H.td $ H.toHtml $ createLink rl
    H.td $ H.toHtml $ revDescription $ fst rl
    where diffLink (a,b) = revId a ++ "_" ++ revId b ++ ".html"
          createLink (a,b) = H.a ! A.href (toValue $ diffLink (a,b)) $ H.toHtml (show (revDateTime a))

compressorCompiler :: String -> Item String -> Compiler (Item String)
compressorCompiler t = withItemBody(unixFilter "yui-compressor" ["--type", t])

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
    -- Render articles
    _ <- ($) match "articles/*" $ version "raw" $ do
        route   routePage
        compile $ pandocCompiler
            -- >>= saveSnapshot "content"
            -- >>= addRevisionList
            >>= loadAndApplyTemplate "templates/post.html" (field "history" (const addRevisionList) <> contentCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" commonContext
            >>= relativizeUrls
    --diffs <- buildDiffsWith "articles/*" (fromCapture "diffs/*.html")

    -- it's a right pain in the arse that you can't use the match function here. would be nice to do something like
    -- match "articles" $ preprocess $ do ids <- getMatches
    -- or something like that.
    -- diffs ends up being Rules [Map Identifier [(Revision,Revision)]]
    diffs <- preprocess $ do
      -- this is a bit brittle. use Hakyll internal function to get the list of all files in here
      -- which we know is where our git repo is
      ids <- getRecursiveContents (const $ return False) "articles"
      let ids' = map (fromFilePath) ids
      mapM buildDiffs ids'
      
    forM_ diffs $ \d -> 
      create (createDiffIdentifiers d) $ do
	route idRoute
	compile $ do
	  path <- toFilePath <$> getUnderlying
	  unsafeCompiler (putStrLn path)
	  revisionList <- getRevisionList
	  diff' <- unsafeCompiler (getDiff path revisionList)
	  constructDiff (takeFileName path) diff'
  
    
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
  --return $ (id', revs)
  let diffMap = M.fromList [(id',revs)]
  return diffMap
 
createDiffIdentifiers :: M.Map Identifier [(Revision, Revision)] -> [Identifier]
createDiffIdentifiers diffs = map fromFilePath (fp diffs) 
  where
    f i (a,b) = i ++ "/" ++ revId a ++ "_" ++ revId b ++ ".html"
    fp = M.foldrWithKey (\k x ks-> ks ++  map (f ((combine "articles") $ dropExtension (toFilePath k))) x) []
  
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

