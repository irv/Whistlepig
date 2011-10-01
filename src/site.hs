{-# LANGUAGE DeriveDataTypeable, Arrows, OverloadedStrings #-}
import Control.Arrow (returnA, (>>>), arr, (***))
import Data.FileStore hiding (create)
import Hakyll
import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!),toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mempty, mconcat, mempty, mappend)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.FilePath (combine, dropExtension, takeFileName)

type Diff = [(DI, [String])]

fileStore :: FileStore
fileStore = gitFileStore "/home/irv/Projects/whistlepig/articles"

getRevisionList :: Compiler String (FilePath, [(Revision, Revision)])
getRevisionList = unsafeCompiler $ \path -> do
    lst <- getRevisions $ takeFileName path
    putStrLn $ show $ lst
    return (path, lst)

getDiff :: Compiler (FilePath, [(Revision, Revision)]) Diff
getDiff = unsafeCompiler $ \(page,rl) -> do
    diffs <- mapM (getFileDiff (takeFileName page) ) rl
    return $ concat diffs

renderDiff :: (DI, [String]) -> String
renderDiff l = renderHtml $ H.pre ! A.class_ (attrCls (fst l)) $ diffCnt l
    where attrCls c   = toValue $ diffStr "diff" c
          diffCnt l'   = H.toHtml $ diffInd (fst l') ++ unlines (snd l')
          diffStr a b = a ++ show b
          diffInd i   = case i of
                           F -> "-"
                           S -> "+"
                           B -> ""

getFileDiff :: FilePath -> (Revision, Revision) -> IO Diff
getFileDiff f a = diff fileStore f (Just $ revId $ fst a) (Just $ revId $ snd a)

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
    >>> arr (setField "diff" (diff' d))
    >>> arr (setField "title" ("Changes " ++ i))
    >>> applyTemplateCompiler "templates/diff.html"
    >>> applyTemplateCompiler "templates/default.html"
    where diff' =  concatMap renderDiff

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

-- makeTagList :: String
--             -> [Page String]
--             -> Compiler () (Page String)
-- makeTagList tag posts =
--     constA (mempty, posts)
--         >>> addPostList
--         >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
--         >>> applyTemplateCompiler "templates/posts.html"
--         >>> applyTemplateCompiler "templates/default.html"
--         >>> relativizeUrlsCompiler

-- addPostList :: Compiler (Page String, [Page String]) (Page String)
-- addPostList = setFieldA "posts" $
--     arr (reverse . chronological)
--         >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
--         >>> arr mconcat
--         >>> arr pageBody

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
    match "files/*" $ do
        route idRoute
        compile copyFileCompiler
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "about/*" $ do
        route $ setExtension ".html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    -- Render articles
    _ <- ($) match "articles/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> addRevisionList
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    group "diffs" $ match "articles/*" $
        metaCompile $ requireAll_ "articles/*"
            >>> mapCompiler makeRevisionCompiler
    match "diffs/*" $ route $ gsubRoute "diffs/" (const "articles/diffs/") `composeRoutes` setExtension "html"
    match "templates/*" $ compile templateCompiler
   -- Index
--     match "index.html" $ route idRoute
--     create "index.html" $ constA mempty
--         >>> arr (setField "title" "Home")
--         >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
--         >>> requireAllA ("articles/*" `mappend` inGroup Nothing) (id *** arr (take 3 . reverse . chronological) >>> addPostList)
--         >>> applyTemplateCompiler "templates/index.html"
--         >>> applyTemplateCompiler "templates/default.html"
--         >>> relativizeUrlsCompiler
        -- Tags
--     _ <- ($) create "tags" $
--         requireAll "articles/*" (\_ ps -> readTags ps :: Tags String)
-- 
--     -- Add a tag list compiler for every tag
--     match "tags/*" $ route $ setExtension ".html"
--     metaCompile $ require_ "tags"
--         >>> arr tagsMap
--         >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))
    where
        tagIdentifier :: String -> Identifier (Page String)
        tagIdentifier = fromCapture "tags/*"
        renderTagCloud' :: Compiler (Tags String) String
        renderTagCloud' = renderTagCloud tagIdentifier 100 120

