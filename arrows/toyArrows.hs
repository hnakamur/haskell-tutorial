{-# LANGUAGE Arrows #-}
 
import Control.Arrow (returnA)
 
idA :: a -> a
idA = proc a -> returnA -< a
 
plusOne :: Int -> Int
plusOne = proc a -> returnA -< (a+1)
 
plusTwo :: Int -> Int
plusTwo = proc a -> plusOne -< (a+1)
 
plusTwoBis :: Int -> Int
plusTwoBis = proc a -> do b <- plusOne -< a
                          plusOne -< b
