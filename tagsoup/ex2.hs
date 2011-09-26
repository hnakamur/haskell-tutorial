{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Search as BS

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike

type MyTag = Tag ByteString

app :: StringLike str => ([Tag str] -> [Tag str]) -> str -> str
app f html = (renderTags . f . myParseTags) html

myParseTags :: StringLike str => str -> [Tag str]
myParseTags = parseTagsOptions $ parseOptionsEntities $ const Nothing

removeLangAttrInHtmlTag :: MyTag -> MyTag
removeLangAttrInHtmlTag (TagOpen "html" attrs) =
    TagOpen "html" (removeAttrNameInAttrs "lang" attrs)
removeLangAttrInHtmlTag tag = tag

removeAttrNameInAttrs :: StringLike str => str -> [(str,str)] -> [(str,str)]
removeAttrNameInAttrs name attrs = [attr | attr <- attrs, fst attr /= name]

replaceNbsp :: MyTag -> MyTag
replaceNbsp (TagText text) = TagText (replace "&nbsp;" " " text)
replaceNbsp tag = tag

replace :: ByteString -> ByteString -> ByteString -> ByteString
replace search rep input = B.concat $ BLC.toChunks $ BS.replace search rep input

html1 = "<html lang=\"en\">\n\
\  <body>Hello<hr/>World.</body>\n\
\</html>"
html1' = app (map removeLangAttrInHtmlTag) html1

html2 = "Hello&nbsp;World"
html2' = app (map replaceNbsp) html2

html3 = "<html lang=\"en\">\n\
\  <body>Hello&nbsp;World.</body>\n\
\</html>"
html3' = app (map (removeLangAttrInHtmlTag . replaceNbsp)) html3
