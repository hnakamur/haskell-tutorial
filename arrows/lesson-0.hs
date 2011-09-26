import Text.XML.HXT.Core

play :: Integer -> IO ()
play arg =
    do { results <- runX (dumb arg)
       ; print results
       }

dumb :: Integer -> IOSArrow XmlTree Integer
dumb n =
    arr (const n) <+> arr (const (n+1))
    >>>
    returnA <+> arr (* 2)
    >>>
    isA (<= 60)
