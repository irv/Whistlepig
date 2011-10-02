{-# LANGUAGE DeriveDataTypeable, Arrows, OverloadedStrings #-}
import Control.Arrow (returnA, (>>>), arr, (***), (>>^))
import Data.FileStore hiding (create)
import Prelude hiding (id)
import Control.Category (id)
import Hakyll
import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!),toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mempty, mconcat, mempty, mappend)
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (TimeLocale, defaultTimeLocale)
import System.FilePath (combine, dropExtension, takeFileName)
import Text.Pandoc (bottomUp, defaultWriterOptions, Pandoc, WriterOptions(..), writeHtmlString, readMarkdown, defaultParserState, Block(Para), Inline(Link))
type Diff = [(DI, [String])]

fileStore :: FileStore
fileStore = gitFileStore "articles"

getRevisionList :: Compiler String (FilePath, [(Revision, Revision)])
getRevisionList = unsafeCompiler $ \path -> do
    lst <- getRevisions $ takeFileName path
    return (path, lst)

getDiff :: Compiler (FilePath, [(Revision, Revision)]) Diff
getDiff = unsafeCompiler $ \(page,rl) -> do
    diffs <- mapM (getFileDiff (takeFileName page) ) rl
    return $ head diffs

renderDiff :: (DI, [String]) -> String
renderDiff l =  diffInd (fst l) ++ unlines (snd l) ++ diffInd (fst l) ++ nl (fst l) --renderHtml $ H.pre ! A.class_ (attrCls (fst l)) $ diffCnt l
    where attrCls c   = toValue $ diffStr "diff" c
          diffCnt l'   = H.toHtml $ diffInd (fst l') ++ unlines (snd l')
          diffStr a b = a ++ show b
          diffInd i   = case i of
                           F -> "~~" -- strikeout
                           S -> "**" -- emphasis
                           B -> ""
          nl i        = case i of
                             F -> "\n"
                             S -> "\n"
                             otherwise -> ""

getFileDiff :: FilePath -> (Revision, Revision) -> IO Diff
getFileDiff f (a,b) = diff fileStore f (Just $ revId b) (Just $ revId a)

getRevisions :: FilePath -> IO [(Revision, Revision)]
getRevisions f = do
    revList <- history fileStore [f] (TimeRange Nothing Nothing)
    return $ makePairList revList
    -- makePairList [1..4] => [(1,2),(2,3),(3,4)]
    where makePairList xs = [(y, x) | x <- xs, y <- xs, y == getListPrev x (reverse xs), x /= y]

getListPrev :: Eq a => a -> [a] -> a
getListPrev i l = l !! checkBounds (fromMaybe 0 (i `elemIndex` l) +1 )
    where checkBounds x | x > length l -1 = length l-1 | otherwise = x

constructDiff :: String -> Diff -> Compiler () (Page String)
constructDiff i d = constA mempty
    >>> addDefaultFields >>> arr applySelf
--     >>> pageCompiler (fromIdentifier $parseIdentifier i) >>> \res page -> do
--         return page
    >>> arr (setField "diff" (writeHtmlString options $ readMarkdown defaultParserState $ diff' d))
    >>> arr (setField "title" ("Changes " ++ i))
    >>> renderTagsField "prettytags" (fromCapture "tags/*")
    >>> applyTemplateCompiler "templates/diff.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
    where diff' =  concatMap renderDiff

applyDiffMarkup :: undefined
applyDiffMarkup = undefined

makeRevisionCompiler ::
        Compiler (Page String)
                (Identifier (Page String), Compiler () (Page String))
makeRevisionCompiler = proc page -> do
        revisionList <- getRevisionList -< (getField "path" page)
        diff' <- getDiff -< revisionList
        let gd = constructDiff (getField "path" page) diff'
        returnA -< (diff_ident revisionList, gd)
    where diff_ident i = parseIdentifier $ ("diffs/" ++ revId ( fst $ head $ snd i)) ++ ("_" ++ revId ( snd $ head $ snd i)) ++ ".markdown"
--     where diff_ident (_, (b,c):_) =  parseIdentifier $ "diffs/" ++ (show $ revId b) ++ "_" ++ (show $ revId c)
--           diff_ident (a, []) = parseIdentifier $ a

routePage :: Routes
routePage = customRoute fileToDirectory

fileToDirectory :: Identifier a -> FilePath
fileToDirectory = flip combine "index.html" . dropExtension . toFilePath

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

addRevisionList :: Compiler (Page String) (Page String)
addRevisionList = unsafeCompiler $ \page -> do
    let path = getField "path" page
    lst <- getRevisions $ takeFileName path
    return $ setField "history" (concatMap renderRevision lst) page

renderRevision :: (Revision, Revision) -> String
renderRevision rl = renderHtml $ H.tr $ do
    H.td $ H.toHtml $ createLink rl
    H.td $ H.toHtml $ revDescription $ fst rl
    where diffLink (a,b) = "./diffs/" ++ revId a ++ "_" ++ revId b ++ ".html"
          createLink (a,b) = H.a ! A.href (toValue $ diffLink (a,b)) $ H.toHtml (show (revDateTime a))

main :: IO ()
main = hakyll $ do
    let static = route idRoute >> compile copyFileCompiler
    mapM_ (`match` static ) ["files/**", "js/lib/*", "images/**"]
    match "js/*.js" $ do
        route idRoute
        compile $ getResourceString >>> unixFilter "yui-compressor" ["--type", "js"]
    match "js/*.coffee" $ do
        route $ setExtension "js"
        compile $ getResourceString
            >>> unixFilter "coffee" ["--compile", "-s"]
            >>> unixFilter "yui-compressor" ["--type", "js"]
    match "css/*.css" $ do
        route   idRoute
        compile $ getResourceString >>> unixFilter "yui-compressor" ["--type", "css"]
    match "css/*.sass" $ do
        route   idRoute
        compile $ getResourceString
            >>> unixFilter "sass" ["-s", "--scss"]
            >>> unixFilter "yui-compressor" ["--type", "css"]
    match "about/*" $ do
        route $ setExtension ".html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    -- Render articles
    _ <- ($) match "articles/*" $ do
        route   routePage
        compile $ pageCompilerWith defaultHakyllParserState options
            >>> renderModificationTime "modified" "%B %e, %Y"
            >>> arr(changeField "date" prettyPrintDate)
            -- >>> copyBodyFromField "date"
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> addRevisionList
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    group "diffs" $ match "articles/*" $
        metaCompileWith "diffs" $ requireAll_ "articles/*"
            >>> mapCompiler makeRevisionCompiler
    match "diffs/*" $ route $( gsubRoute "diffs/" (const "articles/diffs/") `composeRoutes` setExtension "html")
    match "templates/*" $ compile templateCompiler
   -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Andy Irving")
        >>> arr (setField "description" "The personal website of Andy Irving")
        >>> arr (setField "author" "Andy Irving")
        >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
        >>> requireAllA "articles/*" (id *** arr (take 3 . reverse . sortByBaseName) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
        -- Tags
    _ <- ($) create "tags" $
        requireAll "articles/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))
    where
        tagIdentifier :: String -> Identifier (Page String)
        tagIdentifier = fromCapture "tags/*"
        renderTagCloud' :: Compiler (Tags String) String
        renderTagCloud' = renderTagCloud tagIdentifier 100 120

-- Pandoc options
options :: WriterOptions
options = defaultWriterOptions{ writerTableOfContents = True,
                                    writerTemplate = "$if(toc)$\n$toc$\n$endif$\n$body$",
                                    writerWrapText = True,
                                    writerColumns = 72,
                                    writerTabStop = 4,
                                    writerStandalone = True,
                                    writerSectionDivs = True,
                                    writerHtml5 = True,
                                    writerReferenceLinks = False
                                    
}

prettyPrintDate :: String -> String
prettyPrintDate date = fromMaybe defaultValue $ do
        let dateString = intercalate "-" $ take 3 $ splitAll "-" date
        time <- parseTime defaultTimeLocale "%Y-%m-%d" dateString :: Maybe UTCTime
        return $ formatTime defaultTimeLocale format time
    where defaultValue = "Unknown"
          format = "%B %e, %Y"
