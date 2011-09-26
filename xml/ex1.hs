import Text.XML.Light hiding (findChild)
import Text.XML.Light.Cursor
import Data.ByteString as B
import Data.Maybe (fromJust)

main = do
    s <- B.readFile "people.xml"
    let elem = fromJust $ parseXMLDoc s
        c = fromElement elem
    return c

isAnyElement :: Content -> Bool
isAnyElement (Elem _) = True
isAnyElement _ = False

isAnyElementC :: Cursor -> Bool
isAnyElementC = isAnyElement . current

name :: String -> QName
name n = QName { qName = n, qURI = Nothing, qPrefix = Nothing }

removeAttr :: QName -> Element -> Element
removeAttr name elem = elem { elAttribs = attribs }
  where
    attribs = Prelude.filter (not . isAttrNamed name) (elAttribs elem)

isAttrNamed :: QName -> Attr -> Bool
isAttrNamed name attr = attrKey attr == name

element :: Content -> Element
element (Elem e) = e

{-p cursor = Prelude.putStrLn $ showContent $ toTree cursor-}
p cursor = Prelude.putStrLn $ showContent $ current cursor
