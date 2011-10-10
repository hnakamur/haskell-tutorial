import Data.Char (ord)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)
import System.Environment
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Text.HTML.TagSoup.Tree.Zipper as Z
import Text.StringLike
import qualified Data.Text as T

type MyTag = Tag String
type MyTagTree = TagTree String
type MyZipper = Z.TagTreeZipper String
type MyAttr = Attribute String

type AttrsFunc = [Attribute String] -> [Attribute String]

main = do
    [src, dst] <- getArgs
    str <- readFile src
    let trees = tagTree $ parseTags str
        z = Z.toZipper (head trees)
        z2 = walk processZipper z
        tree2 = Z.fromZipper z2
    writeFile dst $ renderTags' (flattenTree [tree2])
    return ()

walk :: (MyZipper -> MyZipper) -> MyZipper -> MyZipper
walk f z
    = let z2 = f z
      in case Z.nextBranch z2 of
             Just z3 -> walk f z3
             Nothing -> z2

processZipper :: MyZipper -> MyZipper
processZipper z
    = let (t, ctx) = removeInvalidHrefLinkTag
                   $ unwrapObjectTag
                   $ removeTag "form" z
      in changeTheadToTbody $ insertTbodyTag (processTree t, ctx)

changeTheadToTbody :: MyZipper -> MyZipper
changeTheadToTbody z@(TagBranch "thead" attrs children, ctx)
    = case Z.nextSiblingBranch z of
          Nothing -> (TagBranch "tbody" attrs children, ctx)
          otherwise -> z
changeTheadToTbody z = z

insertTbodyTag :: MyZipper -> MyZipper
insertTbodyTag z@(TagBranch "thead" attrs children, _)
    = case Z.nextSiblingBranch z of
          Just (TagBranch "tr" _ _, _)
              -> case Z.wrapNextChildren (TagBranch "tbody" [] []) z of
                     Just z2 -> z2
                     Nothing -> error "failed to wrap children with <tbody>"
          otherwise -> z
insertTbodyTag z = z

unwrapObjectTag :: MyZipper -> MyZipper
unwrapObjectTag z@(TagBranch "object" _ _, _) = fromJust $ Z.unwrap z
unwrapObjectTag z = z

removeTag :: String -> MyZipper -> MyZipper
removeTag name z@(TagBranch name' _ _, _) | name == name'
    = case Z.remove z of
          Just z2 -> z2
          Nothing -> z
removeTag _ z = z

removeInvalidHrefLinkTag :: MyZipper -> MyZipper
removeInvalidHrefLinkTag z@((TagBranch "link" attrs _), _)
    = case lookup "href" attrs of
          Just val
              -> if "data:text/css," `isPrefixOf` val
                 then case Z.remove z of
                          Just z2 -> z2
                          Nothing -> z
                 else z
          Nothing -> z
removeInvalidHrefLinkTag z = z

processTree :: MyTagTree -> MyTagTree
processTree t@(TagBranch name attrs children)
    = replaceColonsInHref
    $ replaceColonsInId
    $ modifyAttrs (removeAttrsByNames ["lang"])
    $ case name of
          "html" -> modifyAttrs (removeAttrsByNames ["lang", "class"]) t
          "li" -> modifyAttrs (removeAttrsByNames ["value"]) t
          "link" -> modifyAttrs (replaceAttrValExact "href"
                        "http://www.w3.org/StyleSheets/TR/W3C-ED" "W3C-ED") t
          "img" -> modifyAttrs (replaceAttrValExact "src"
                        "http://www.w3.org/Icons/w3c_home"
                        "images/w3c_home.png") t
          "a" -> modifyAttrs (
                     (replaceAttrValExact "href"
                        "infrastructure.html#conforming-documents"
                        "infrastructure.html#conforming-html5-documents"
                     ) .
                     (replaceAttrValExact "href"
                        "Overview.html"
                        "http://dev.w3.org/html5/spec/Overview.html"
                     )
                  ) t
          otherwise -> t
processTree t@(TagLeaf _) = t

replaceColonsInHref :: MyTagTree -> MyTagTree
replaceColonsInHref = modifyAttrs $ modifyAttrVal "href" replaceColons

replaceColons :: String -> String
replaceColons
    = replaceExceptPrefixes
          (replace ":" "-")
          ["mailto:", "http:", "https:", "data:"]                  

replaceExceptPrefixes :: (String -> String) -> [String] -> String -> String
replaceExceptPrefixes func [] input = func input
replaceExceptPrefixes func (prefix:xs) input
    = case replaceExceptPrefix func prefix input of
          Just result -> result
          Nothing -> replaceExceptPrefixes func xs input

replaceExceptPrefix :: (String -> String) -> String -> String -> Maybe String
replaceExceptPrefix func prefix input
    = case stripPrefix prefix input of
          Just rest -> Just (prefix ++ func rest)
          Nothing -> Nothing

replaceColonsInId :: MyTagTree -> MyTagTree
replaceColonsInId = modifyAttrs (replaceAttrVal "id" ":" "-")

modifyAttrs f (TagBranch name attrs children)
    = (TagBranch name (f attrs) children)

removeAttrsByNames :: [String] -> [MyAttr] -> [MyAttr]
removeAttrsByNames names attrs = filter f attrs
  where
    f (name,val) = name `notElem` names

modifyAttrVal :: String -> (String -> String) -> [MyAttr] -> [MyAttr]
modifyAttrVal attrName func attrs = map f attrs
  where
    f (name,val) | name == attrName = (name, func val)
                 | otherwise = (name, val)

replaceAttrValExact :: String -> String -> String -> [MyAttr] -> [MyAttr]
replaceAttrValExact attrName search rep attrs = map f attrs
  where
    f (name,val) | name == attrName = (name, replaceExact search rep val)
                 | otherwise = (name, val)

replaceExact :: String -> String -> String -> String
replaceExact search rep input | search == input = rep
                              | otherwise = input

replaceAttrVal :: String -> String -> String -> [MyAttr] -> [MyAttr]
replaceAttrVal attrName search rep attrs = map f attrs
  where
    f (name,val) | name == attrName = (name, replace search rep val)
                 | otherwise = (name, val)

replace :: String -> String -> String -> String
replace search rep input
    = T.unpack $ T.replace (T.pack search) (T.pack rep) (T.pack input)

escapeNonAscii = concatMap escapeNonAsciiChar

escapeNonAsciiChar c | c > '\x7f' = "&#" ++ show (ord c) ++ ";"
                     | otherwise = [c]


removeAttrNameInAttrs :: StringLike str => str -> [(str,str)] -> [(str,str)]
removeAttrNameInAttrs name attrs = [attr | attr <- attrs, fst attr /= name]

removeAttrs :: StringLike str => [str] -> [(str,str)] -> [(str,str)]
removeAttrs names attrs = [attr | attr <- attrs, fst attr `notElem` names]

replaceNbsp :: MyTag -> MyTag
replaceNbsp (TagText text) = TagText (replace "&nbsp;" " " text)
replaceNbsp tag = tag


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
