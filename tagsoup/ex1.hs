import Text.HTML.TagSoup
import Text.HTML.TagSoup.Parsec
import Text.ParserCombinators.Parsec
import Text.StringLike

html1 = "<html lang=\"en\">\
\  <body>Hello</body>\
\</html>"

f1 = tParse (openTag "html2") (parseTags html1)

run p = tParse p (parseTags html1)

{-mytoken :: (StringLike str,Show str)
           => (Tag str -> Maybe (Tag str)) -> TagParserSt str st (Tag str)-}
{-mytoken :: (StringLike str,Show str)
           => (Tag str -> Maybe (Tag str)) -> TagParser str (Tag str)-}
mytoken test = token showToken posToken testToken
  where
    showToken (pos,tok) = show tok
    posToken (pos,tok) = pos
    testToken (pos,tok) = test tok

{-anyTag :: (StringLike str,Show str) => TagParserSt str st (Tag str)
anyTag = mytoken $ (\tok -> Just tok)-}
