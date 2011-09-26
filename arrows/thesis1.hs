import Control.Arrow ((&&&), (>>>), ArrowPlus(..), arr)
import Control.Arrow.ArrowList (ArrowList(..))
import Control.Arrow.ListArrow (LA(..))
import Data.List (sort)

isTwo :: Int -> Bool
isTwo n = n == 2

thisIsTwo :: Int -> [Int]
thisIsTwo a = runLA (isA isTwo) a

addSomething :: (ArrowList a) => Int -> a Int Int
addSomething x = arr (+x)

testAdd :: [Int]
testAdd = runLA (addSomething 2) 2

testAdd' :: [Int]
testAdd' = runLA (arr2A addSomething) (2,2)

f2_11 :: [Int]
f2_11 = runLA ((constA 2 <+> constA 4) >>. reverse) []

collectAndSort :: (ArrowList cat, Ord c) => cat a c -> cat a c
collectAndSort collect = listA collect >>> arrL sort

collectAndSortEx1 :: [Int]
collectAndSortEx1 = runLA (collectAndSort (constA 4 <+> constA 2 <+> constA 3)) []

collectAndSortEx2 :: [Int]
collectAndSortEx2 = runLA (collectAndSort (constA 4 >>> constA 2 >>> constA 3)) []

arr2LEx1 :: [Int]
arr2LEx1 = runLA (constA 2 &&& constA 4 >>> arr2L (\x y -> [x + y])) []

arr2ex1 :: [Int]
arr2ex1 = runLA (constA 2 &&& constA 4 >>> arr2 (+)) []

catAEx1 :: [Int]
catAEx1 = runLA (collectAndSort (catA [constA 3, constA 1, constA 5])) []
