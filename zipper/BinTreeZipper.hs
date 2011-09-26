import Data.Maybe (fromJust)

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)

freeTree :: Tree Char
freeTree
    = Node 'P'
          (Node 'O'
              (Node 'L'
                  (Node 'N' Empty Empty)
                  (Node 'T' Empty Empty)
              )
              (Node 'Y'
                  (Node 'S' Empty Empty)
                  (Node 'A' Empty Empty)
              )
          )
          (Node 'L'
              (Node 'W'
                  (Node 'C' Empty Empty)
                  (Node 'R' Empty Empty)
              )
              (Node 'A'
                  (Node 'A' Empty Empty)
                  (Node 'C' Empty Empty)
              )
          )

coolTree = Node 1 Empty (Node 3 Empty Empty)

data Direction = L | R
               deriving (Show)
{-type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x
-}

{-type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)-}

x -: f = f x

data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb a (Tree a)
             deriving (Show)
type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)
type ErrorOrZipper a = Either String (Zipper a)

goLeft :: Zipper a -> ErrorOrZipper a
goLeft (Node x l r, bs) = Right (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Left "Cannot go left"

goRight :: Zipper a -> ErrorOrZipper a
goRight (Node x l r, bs) = Right (r, RightCrumb x l:bs)
goRight (Empty, _) = Left "Cannot go right"

goUp :: Zipper a -> ErrorOrZipper a
goUp (t, LeftCrumb x r:bs) = Right (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Right (Node x l t, bs)
goUp (_, []) = Left "Cannot go up"

modify :: (a -> a) -> Zipper a -> ErrorOrZipper a
modify f (Node x l r, bs) = Right (Node (f x) l r, bs)
modify f (Empty, bs) = Left "Cannot modify empty tree"

attach :: Tree a -> Zipper a -> ErrorOrZipper a
attach t (Empty, bs) = Right (t, bs)
attach t (Node _ _ _, _) = Left "Cannot attach to non-empty tree"

detach :: Zipper a -> ErrorOrZipper a
detach (_, LeftCrumb x r:bs) = Right (Node x Empty r, bs)
detach (_, RightCrumb x l:bs) = Right (Node x l Empty, bs)
detach (_, []) = Right (Empty, [])

topMost :: Zipper a -> ErrorOrZipper a
topMost (t, []) = Right (t, [])
topMost z = goUp z >>= topMost
