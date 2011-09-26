{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Search as BS

import System.Environment
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike

type MyTag = Tag ByteString
type AttrsFunc = [Attribute ByteString] -> [Attribute ByteString]

{-main = do
    [src, dst] <- getArgs
    str <- B.readFile src
    B.writeFile dst $
        filterAscii (app (map processTagFunc) str)
    return ()-}

main = do
    [src, dst] <- getArgs
    str <- B.readFile src
    let tags = map processTagFunc (parseTags str)
        tree = tagTree tags
    B.writeFile dst $
        filterAscii (renderTags' (flattenTree (processTreeFunc tree)))
    return ()

processTagFunc
    = removeAttrsInTag "html" ["lang", "class"]
    . replaceAttrVal "a" "href"
        "mailto-" "mailto:"
    . replaceAttrVal "a" "href"
        "http-//" "http://"
    . replaceAttrVal "a" "href"
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

removeTagInTrees :: Eq str => str -> [TagTree str] -> [TagTree str]
removeTagInTrees tagName = concatMap $ removeTagInTree tagName

removeTagInTree :: Eq str => str -> TagTree str -> [TagTree str]
removeTagInTree tagName (TagBranch name attrs trees)
    | tagName == name = []
    | otherwise = [TagBranch name attrs (removeTagInTrees tagName trees)]
removeTagInTree tagName leaf = [leaf]

tagTreeBranchNameLit :: Eq str => str -> TagTree str -> Bool
tagTreeBranchNameLit name (TagBranch name' attrs trees) | name == name' = True
tagTreeBranchNameLit _ _ = False

app :: StringLike str => ([Tag str] -> [Tag str]) -> str -> str
app f html = (renderTags . f . myParseTags) html

filterAscii = B.filter (\c -> c <= 0x7f)

myParseTags :: StringLike str => str -> [Tag str]
myParseTags = parseTagsOptions $ parseOptionsEntities $ const Nothing

procAttrs :: ByteString -> AttrsFunc -> MyTag -> MyTag
procAttrs tagName f (TagOpen tagName' attrs) | tagName == tagName'
    = TagOpen tagName (f attrs)
procAttrs _ _ tag = tag

removeAttrInTag :: ByteString -> ByteString -> MyTag -> MyTag
removeAttrInTag tagName attrName (TagOpen tagName' attrs)
      | tagName == tagName' =
    TagOpen tagName (removeAttrNameInAttrs attrName attrs)
removeAttrInTag _ _ tag = tag

{-removeAttrsInTag :: ByteString -> [ByteString] -> MyTag -> MyTag
removeAttrsInTag tagNm attrNames (TagOpen tagNm' attrs) | tagNm == tagNm'
    = TagOpen tagNm (removeAttrs attrNames attrs)
removeAttrsInTag _ _ tag = tag-}

removeAttrsInTag :: ByteString -> [ByteString] -> MyTag -> MyTag
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

replace :: ByteString -> ByteString -> ByteString -> ByteString
replace search rep input = B.concat $ BLC.toChunks $ BS.replace search rep input

replaceExact :: ByteString -> ByteString -> ByteString -> ByteString
replaceExact search rep input | search == input = rep
                              | otherwise = input

replaceAttrVal :: ByteString -> ByteString -> ByteString -> ByteString -> MyTag -> MyTag
replaceAttrVal tagName attrName search rep
    = procAttrs tagName (map f)
  where
    f (name,val) | name == attrName = (name, replace search rep val)
                 | otherwise = (name, val)

replaceAttrValExact :: ByteString -> ByteString -> ByteString -> ByteString -> MyTag -> MyTag
replaceAttrValExact tagName attrName search rep
    = procAttrs tagName (map f)
  where
    f (name,val) | name == attrName = (name, replaceExact search rep val)
                 | otherwise = (name, val)

escapeHTML' :: ByteString -> ByteString
escapeHTML' = filterAscii . escapeHTML

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

html4 :: ByteString
html4 = "<p>The <code title=\"\">javascript:</code> URL</p>"
html4tags = parseTags html4

html5 :: ByteString
html5 = "<title>Acknowledgements &#8212; HTML5</title>"
html5tags = myParseTags html5
