import Text.XML.HXT.Core

play arg = runX (processor arg)

processor :: FilePath -> IOSArrow XmlTree XmlTree
processor filename =
    readDocument [withValidate no] filename >>>
    putXmlTree "-"
