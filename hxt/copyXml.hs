module Main
where
 
import Text.XML.HXT.Core
import Text.XML.HXT.Curl -- use libcurl for HTTP access
                         -- only necessary when reading http://...
 
import System.Environment
 
main :: IO ()
main = do{
      [src, dst] <- getArgs;
      runX ( readDocument [withValidate no
                          ,withCurl []
                          ] src
       >>>
       writeDocument [withIndent yes
                           ,withOutputEncoding utf8
                           ] dst
      );
      return ()
    }
