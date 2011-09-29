{-# LANGUAGE DeriveDataTypeable, Arrows, OverloadedStrings #-}
import Control.Arrow (returnA, (>>>), arr)
import Data.FileStore hiding (create)
import Hakyll
import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!),toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mempty, mconcat, mappend, mempty)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.FilePath (combine, dropExtension)
import Data.Typeable (Typeable)
-- seems odd this isn't in Data.FileStore
instance Ord Revision where
        compare x y
         | revDateTime x == revDateTime y    =  EQ
         | revDateTime x <= revDateTime y    =  LT
         | otherwise                         =  GT

fileStore :: FileStore
fileStore = gitFileStore "/home/irv/Projects/whistlepig"

getRevisionList :: Compiler (Identifier (Page String)) (FilePath, [(Revision, Revision)])
getRevisionList = unsafeCompiler $ \page -> do
    let path = toFilePath page
    revs <- getRevisions path
    return (path, cartProd revs)
    -- cartProd [1..4] => [(1,2),(2,3),(3,4)]
    where cartProd xs = [(y, x) | x <- xs, y <- xs, y == getListPrev x (reverse xs), x /= y]

getDiff :: Compiler (FilePath, [(Revision, Revision)]) [(DI, [String])]
getDiff = unsafeCompiler $ \(page,rl) -> do
    diffs <- mapM (getFileDiff page ) rl
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

getFileDiff :: FilePath -> (Revision, Revision) -> IO [(DI, [String])]
getFileDiff f a = diff fileStore f (Just $ revId $ fst a) (Just $ revId $ snd a)

getRevisions :: FilePath -> IO [Revision]
getRevisions f = history fileStore [f] (TimeRange Nothing Nothing)

getListPrev :: Eq a => a -> [a] -> a
getListPrev i l = l !! checkBounds (fromMaybe 0 (i `elemIndex` l) +1 )
    where checkBounds x | x > length l -1 = length l-1 | otherwise = x

constructDiff :: [(DI, [String])] -> Compiler () (Page String)
constructDiff d = constA mempty
    >>> arr (setField "title" "Diff")
    >>> applyTemplateCompiler "templates/diff.html"
    >>> relativizeUrlsCompiler

makeRevisionCompiler ::
        Compiler (Page String)
                (Identifier (Page String), Compiler () (Page String))
makeRevisionCompiler = proc page -> do
        identifier <- getIdentifier -< ()
        revisionList <- getRevisionList -< identifier
        diff' <- getDiff -< revisionList
        let gd = constructDiff diff'
        returnA -< (diff_ident revisionList, gd)
    where diff_ident (_, [(b,c)]) = parseIdentifier $ (show $ revId b) ++ "_" ++ (show $ revId c)
          diff_ident (_, []) = parseIdentifier $ ""

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

main :: IO ()
main = hakyll $ do
    -- Render articles
    match "articles/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    create "diffs" $ requireAll "articles/*" (\p t -> map (applyTemplate t) p)
    match "diffs" $ route $ setExtension ".html" $
        metaCompileWith "diffs"$ requireAll_ "articles/*"
            >>> mapCompiler makeRevisionCompiler
    match "templates/*" $ compile templateCompiler
    
        -- Tags
    create "tags" $
        requireAll "articles/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))
    where
        tagIdentifier :: String -> Identifier (Page String)
        tagIdentifier = fromCapture "tags/*"

