import Data.Char (ord)
import System.Environment
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike
import qualified Data.Text as T

type MyTag = Tag String
type MyTagTree = TagTree String
type AttrsFunc = [Attribute String] -> [Attribute String]

{-main = do
    [src, dst] <- getArgs
    str <- readFile src
    writeFile dst $
        filterAscii (app (map processTagFunc) str)
    return ()-}

main = do
    [src, dst] <- getArgs
    str <- readFile src
    let tags = map processTagFunc (parseTags str)
        tree = tagTree tags
    writeFile dst $
        renderTags' (flattenTree (processTreeFunc tree))
    return ()

processTagFunc
    = removeAttrsInTag "html" ["lang", "class"]
    . removeAttrsInTag "span" ["lang"]
    . removeAttrsInTag "pre" ["lang"]
    . removeAttrsInTag "cite" ["lang"]
    . removeAttrsInTag "bdo" ["lang"]
    . removeAttrsInTag "li" ["value"]
    . replaceAttrVal "a" "href"
        "mailto-" "mailto:"
    . replaceAttrVal "a" "href"
        "http-//" "http://"
    . replaceAttrVal "a" "href"
        ":" "-"
    . replaceAttrValInAnyTag "id"
        ":" "-"
    . replaceAttrValExact "link" "href"
        "http://www.w3.org/StyleSheets/TR/W3C-ED" "W3C-ED"
    . replaceAttrValExact "img" "src"
        "http://www.w3.org/Icons/w3c_home" "images/w3c_home.png"
    . replaceAttrValExact "a" "href"
        "Overview.html" "spec.html"
    . replaceNbsp

processTreeFunc
    = removeTagInTrees "form"
    . (walkInTrees "object" unwrapObjectTag)
    . (walkInTrees "table" insertTbody)

walkInTrees :: String -> ([MyTagTree] -> [MyTagTree]) -> [MyTagTree] -> [MyTagTree]
walkInTrees tagName f = concatMap (walkInTree tagName f)

walkInTree :: String -> ([MyTagTree] -> [MyTagTree]) -> MyTagTree -> [MyTagTree]
walkInTree tagName f (TagBranch name attrs trees)
    | tagName == name = f [TagBranch name attrs trees]
    | otherwise = [TagBranch name attrs (walkInTrees tagName f trees)]
walkInTree tagName f leaf@(TagLeaf _) = [leaf]

removeTagInTrees :: Eq str => str -> [TagTree str] -> [TagTree str]
removeTagInTrees tagName = concatMap $ removeTagInTree tagName

removeTagInTree :: Eq str => str -> TagTree str -> [TagTree str]
removeTagInTree tagName (TagBranch name attrs trees)
    | tagName == name = []
    | otherwise = [TagBranch name attrs (removeTagInTrees tagName trees)]
removeTagInTree tagName leaf@(TagLeaf _) = [leaf]

tagTreeBranchNameLit :: Eq str => str -> TagTree str -> Bool
tagTreeBranchNameLit name (TagBranch name' attrs trees) | name == name' = True
tagTreeBranchNameLit _ _ = False

app :: StringLike str => ([Tag str] -> [Tag str]) -> str -> str
app f html = (renderTags . f . myParseTags) html

escapeNonAscii = concatMap escapeNonAsciiChar

escapeNonAsciiChar c | c > '\x7f' = "&#" ++ show (ord c) ++ ";"
                     | otherwise = [c]

myParseTags :: StringLike str => str -> [Tag str]
myParseTags = parseTagsOptions $ parseOptionsEntities $ const Nothing

procAttrs :: String -> AttrsFunc -> MyTag -> MyTag
procAttrs tagName f (TagOpen tagName' attrs) | tagName == tagName'
    = TagOpen tagName (f attrs)
procAttrs _ _ tag = tag

procAttrsInAnyTag :: AttrsFunc -> MyTag -> MyTag
procAttrsInAnyTag f (TagOpen tagName attrs) = TagOpen tagName (f attrs)
procAttrsInAnyTag _ tag = tag

removeAttrInTag :: String -> String -> MyTag -> MyTag
removeAttrInTag tagName attrName (TagOpen tagName' attrs)
      | tagName == tagName' =
    TagOpen tagName (removeAttrNameInAttrs attrName attrs)
removeAttrInTag _ _ tag = tag

{-removeAttrsInTag :: String -> [String] -> MyTag -> MyTag
removeAttrsInTag tagNm attrNames (TagOpen tagNm' attrs) | tagNm == tagNm'
    = TagOpen tagNm (removeAttrs attrNames attrs)
removeAttrsInTag _ _ tag = tag-}

removeAttrsInTag :: String -> [String] -> MyTag -> MyTag
removeAttrsInTag tagName attrNames = procAttrs tagName (removeAttrs attrNames)

removeLangAttrInHtmlTag :: MyTag -> MyTag
removeLangAttrInHtmlTag = removeAttrInTag "html" "lang"

removeAttrNameInAttrs :: StringLike str => str -> [(str,str)] -> [(str,str)]
removeAttrNameInAttrs name attrs = [attr | attr <- attrs, fst attr /= name]

removeAttrs :: StringLike str => [str] -> [(str,str)] -> [(str,str)]
removeAttrs names attrs = [attr | attr <- attrs, fst attr `notElem` names]

replaceNbsp :: MyTag -> MyTag
replaceNbsp (TagText text) = TagText (replace "&nbsp;" " " text)
replaceNbsp tag = tag

replace :: String -> String -> String -> String
replace search rep input
    = T.unpack $ T.replace (T.pack search) (T.pack rep) (T.pack input)

replaceExact :: String -> String -> String -> String
replaceExact search rep input | search == input = rep
                              | otherwise = input

replaceAttrVal :: String -> String -> String -> String -> MyTag -> MyTag
replaceAttrVal tagName attrName search rep
    = procAttrs tagName (map f)
  where
    f (name,val) | name == attrName = (name, replace search rep val)
                 | otherwise = (name, val)

replaceAttrValInAnyTag :: String -> String -> String -> MyTag -> MyTag
replaceAttrValInAnyTag attrName search rep
    = procAttrsInAnyTag (map f)
  where
    f (name,val) | name == attrName = (name, replace search rep val)
                 | otherwise = (name, val)

replaceAttrValExact :: String -> String -> String -> String -> MyTag -> MyTag
replaceAttrValExact tagName attrName search rep
    = procAttrs tagName (map f)
  where
    f (name,val) | name == attrName = (name, replaceExact search rep val)
                 | otherwise = (name, val)

escapeHTML' :: String -> String
escapeHTML' = escapeNonAscii . escapeHTML

renderTags' = renderTagsOptions' renderOptions'

renderOptions' = renderOptions{optEscape = escapeHTML'}

renderTagsOptions' :: StringLike str => RenderOptions str -> [Tag str] -> str
renderTagsOptions' opts = strConcat . tags
    where
        s = fromString
        ss x = [s x]
    
        tags (TagOpen name atts:TagClose name2:xs)
            | name == name2 && optMinimize opts name = open name atts (s " /") ++ tags xs
        tags (TagOpen name atts:xs) | Just ('?',_) <- uncons name = open name atts (s " ?") ++ tags xs
        tags (x:xs) = tag x ++ tags xs
        tags [] = []

        tag (TagOpen name atts) = open name atts (s "")
        tag (TagClose name) = [s "</", name, s ">"]
        tag (TagText text) = [txt text]
        tag (TagComment text) = ss "<!--" ++ com text ++ ss "-->"
        tag _ = ss ""

        txt = optEscape opts
        open name atts shut = [s "<",name] ++ concatMap att atts ++ [shut,s ">"]
        att (x,y) | xnull && ynull = [s " \"\""]
                  {-| ynull = [s " ", x]-}
                  | xnull = [s " \"",txt y,s "\""]
                  | otherwise = [s " ",x,s "=\"",txt y,s "\""]
            where (xnull, ynull) = (strNull x, strNull y)

        com xs | Just ('-',xs) <- uncons xs, Just ('-',xs) <- uncons xs, Just ('>',xs) <- uncons xs = s "-- >" : com xs
        com xs = case uncons xs of
            Nothing -> []
            Just (x,xs) -> fromChar x : com xs


unwrapObjectTag :: [MyTagTree] -> [MyTagTree]
unwrapObjectTag = concatMap unwrapObjectTagSub

unwrapObjectTagSub :: MyTagTree -> [MyTagTree]
unwrapObjectTagSub (TagBranch "object" _ trees) = trees
unwrapObjectTagSub t = [t]


html1 = "<html lang=\"en\">\n\
\  <body>Hello<hr></hr>World.</body>\n\
\</html>"
html1' = app (map removeLangAttrInHtmlTag) html1
html1tree = tagTree $ parseTags html1

html2 = "Hello&nbsp;World"
html2' = app (map replaceNbsp) html2

html3 = "<html lang=\"en\">\n\
\  <body>Hello&nbsp;World.</body>\n\
\</html>"
html3' = app (map (removeLangAttrInHtmlTag . replaceNbsp)) html3
html3tree = tagTree $ parseTags html3

html4 :: String
html4 = "<p>The <code title=\"\">javascript:</code> URL</p>"
html4tags = parseTags html4

html5 :: String
html5 = "<title>Acknowledgements &#8212; HTML5</title>"
html5tags = myParseTags html5

html6 :: String
html6 = "<table><thead><tr><td>1</td></tr></thead> <tr><td>2</td></tr><tr><td>3</td></tr></table>"
html6tags = myParseTags html6
html6tree = tagTree html6tags

html7 :: String
html7 = "<thead><tr><td>1</td></tr></thead> <tr><td>2</td></tr><tr><td>3</td></tr>"
html7tags = myParseTags html7
html7tree = tagTree html7tags

html8 :: String
html8 = "<p>Hello</p>\n\
\<table> \n\
\<thead>\n\
\<tr>\n\
\<th>title</th>\n\
\</tr> \n\
\</thead>\n\
\<tr>         \n\
\<td>1</td>\n\
\</tr>              \n\
\<tr>         \n\
\<td>2</td>\n\
\</tr>              \n\
\</table>\n"
html8tags = myParseTags html8
html8tree = tagTree html8tags

isBranch :: MyTagTree -> Bool
isBranch (TagBranch _ _ _) = True
isBranch (TagLeaf _ ) = False

subTree (TagBranch _ _ t) = t

isLeaf :: MyTagTree -> Bool
isLeaf (TagBranch _ _ _) = False
isLeaf (TagLeaf _ ) = True

insertTbody :: [MyTagTree] -> [MyTagTree]
insertTbody [] = []
insertTbody (x:xs)
  = case x of
      TagBranch "table" attrs trees
        -> TagBranch "table" attrs (insertTbodySub trees) : xs
      otherwise -> x : (insertTbody xs)

insertTbodySub :: [MyTagTree] -> [MyTagTree]
insertTbodySub [] = []
insertTbodySub xs
  = case break isBranch xs of
      (leaves, thead@(TagBranch "thead" attrs trees) : xs')
        -> case break isBranch xs' of
             (leaves', rest@(TagBranch "tr" _ _ : _))
                   -> leaves ++ (thead : (leaves' ++ [TagBranch "tbody" [] rest]))
             otherwise -> xs
      otherwise -> xs

{-insertTbodySub :: [MyTagTree] -> [MyTagTree]
insertTbodySub [] = []
insertTbodySub (x:xs)
  = case x of
      TagBranch "thead" attrs trees
        -> case xs of
             [] -> [x]
             otherwise -> case break isBranch xs of
               (leaves, (TagBranch "tr" _ _ : ys))
                   -> x : (leaves ++ [TagBranch "tbody" [] xs])
               otherwise -> x : xs
      otherwise -> x : xs-}
