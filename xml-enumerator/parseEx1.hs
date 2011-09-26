{-# LANGUAGE OverloadedStrings #-}
import Text.XML.Stream.Parse
import Data.Text.Lazy (Text, unpack)

data Person = Person { age :: Int, name :: Text }
    deriving Show

parsePerson = tagName "person" (requireAttr "age") $ \age -> do
    name <- content
    return $ Person (read $ unpack age) name

parsePeople = tagNoAttr "people" $ many parsePerson

main = parseFile_ (def :: ParseSettings) "people.xml" $ force "people required" parsePeople
