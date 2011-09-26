import Text.XML
import Text.XML.Cursor
import Prelude hiding (writeFile)

{-main = readFile_ def "people.xml"-}

{-main = do
  doc <- readFile_ def "people.xml"
  writeFile def "a.xml" doc-}

main = do
  doc <- readFile_ def "people.xml"
  writeFile def "a.xml" doc
