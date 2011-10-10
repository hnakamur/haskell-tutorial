module Text.HTML.TagSoup.Tree.Zipper
    ( TagTreeZipper
    , toZipper
    , fromZipper
    , getTree
    , setTree
    , replace
    , insert
    , append
    , remove
    , wrapNextChildren
    , unwrap
    , firstChild
    , lastChild
    , parent
    , nextSibling
    , prevSibling
    , root
    , isRoot
    , isBranch
    , isLeaf
    , nextSiblingTill
    , prevSiblingTill
    , nextSiblingBranch
    , prevSiblingBranch
    , firstChildBranch
    , lastChildBranch
    , nextBranch
    ) where

import Control.Monad (void)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

data TagTreeContext str
    = Root
    | Branch str [Attribute str]
          [TagTree str] (TagTreeContext str) [TagTree str]
    deriving (Show)

type TagTreeZipper str = (TagTree str, TagTreeContext str)

toZipper :: TagTree str -> TagTreeZipper str
toZipper t = (t, Root)

fromZipper :: TagTreeZipper str -> TagTree str
fromZipper z = let Just (t, Root) = root z
               in t

getTree :: TagTreeZipper str -> TagTree str
getTree (t, _) = t

setTree :: TagTree str -> TagTreeZipper str -> TagTreeZipper str
setTree t (_, c) = (t, c)

replace :: TagTree str -> TagTreeZipper str -> Maybe (TagTreeZipper str)
replace t z = Just (setTree t z)

insert :: TagTree str -> TagTreeZipper str -> Maybe (TagTreeZipper str)
insert t2 (t, Branch n as ls ctx rs) = Just (t2, Branch n as ls ctx (t:rs))
insert _ _ = Nothing

append :: TagTree str -> TagTreeZipper str -> Maybe (TagTreeZipper str)
append t2 (t, Branch n as ls ctx rs) = Just (t2, Branch n as (t:ls) ctx rs)
append _ _ = Nothing

remove :: TagTreeZipper str -> Maybe (TagTreeZipper str)
remove (_, Branch n as ls ctx (r:rs)) = Just (r, Branch n as ls ctx rs)
remove (_, Branch n as ls ctx []) = Just (TagBranch n as (reverse ls), ctx)
remove _ = Nothing

wrapNextChildren :: TagTree str -> TagTreeZipper str -> Maybe (TagTreeZipper str)
wrapNextChildren (TagBranch n2 a2 _) (t, Branch n as ls ctx rs) =
    Just (TagBranch n2 a2 rs, Branch n as (t:ls) ctx [])
wrapNextChildren _ _ = Nothing

unwrap :: TagTreeZipper str -> Maybe (TagTreeZipper str)
unwrap z@(TagBranch _ _ [], Branch n as ls ctx rs) = remove z
unwrap (TagBranch _ _ (c:cs), Branch n as ls ctx rs) =
    Just (c, Branch n as ls ctx (cs++rs))
unwrap _ = Nothing

firstChild :: TagTreeZipper str -> Maybe (TagTreeZipper str)
firstChild (TagBranch n as (c:cs), ctx) = Just (c, Branch n as [] ctx cs)
firstChild _ = Nothing

lastChild :: TagTreeZipper str -> Maybe (TagTreeZipper str)
lastChild (TagBranch n as cs@(_:_), ctx)
    = Just (last cs, Branch n as (tail (reverse cs)) ctx [])
lastChild _ = Nothing

parent :: TagTreeZipper str -> Maybe (TagTreeZipper str)
parent (_, Root) = Nothing
parent (t, Branch n as ls ctx rs)
    = Just (TagBranch n as (reverse ls++[t]++rs), ctx)

nextSibling :: TagTreeZipper str -> Maybe (TagTreeZipper str)
nextSibling (t, Branch n as ls ctx (r:rs)) = Just (r, Branch n as (t:ls) ctx rs)
nextSibling _ = Nothing

prevSibling :: TagTreeZipper str -> Maybe (TagTreeZipper str)
prevSibling (t, Branch n as (l:ls) ctx rs) = Just (l, Branch n as ls ctx (t:rs))
prevSibling _ = Nothing

root :: TagTreeZipper str -> Maybe (TagTreeZipper str)
root (t, Root) = Just (t, Root)
root (t, z) = parent (t, z) >>= root

isRoot :: TagTreeZipper str -> Bool
isRoot (_, Root) = True
isRoot _ = False

isBranch :: TagTreeZipper str -> Bool
isBranch (TagBranch _ _ _, _) = True
isBranch _ = False

isLeaf :: TagTreeZipper str -> Bool
isLeaf (TagLeaf _, _) = True
isLeaf _ = False

nextSiblingTill :: (TagTreeZipper str -> Bool) -> TagTreeZipper str
               -> Maybe (TagTreeZipper str)
nextSiblingTill pred z = nextSibling z >>= \z' ->
    if pred z' then Just z' else nextSiblingTill pred z'

prevSiblingTill :: (TagTreeZipper str -> Bool) -> TagTreeZipper str
               -> Maybe (TagTreeZipper str)
prevSiblingTill pred z = prevSibling z >>= \z' ->
    if pred z' then Just z' else prevSiblingTill pred z'

nextSiblingBranch :: TagTreeZipper str -> Maybe (TagTreeZipper str)
nextSiblingBranch = nextSiblingTill isBranch

prevSiblingBranch :: TagTreeZipper str -> Maybe (TagTreeZipper str)
prevSiblingBranch = prevSiblingTill isBranch

firstChildBranch :: TagTreeZipper str -> Maybe (TagTreeZipper str)
firstChildBranch z = firstChild z >>= \z' ->
    if isBranch z' then Just z' else nextSiblingBranch z'

lastChildBranch :: TagTreeZipper str -> Maybe (TagTreeZipper str)
lastChildBranch z = lastChild z >>= \z' ->
    if isBranch z' then Just z' else prevSiblingBranch z'

nextBranch :: TagTreeZipper str -> Maybe (TagTreeZipper str)
nextBranch z =
    case firstChildBranch z of
        Just z2 -> Just z2
        Nothing -> case nextSiblingBranch z of
                       Just z3 -> Just z3
                       Nothing -> upNextSiblingBranch z

upNextSiblingBranch :: TagTreeZipper str -> Maybe (TagTreeZipper str)
upNextSiblingBranch z =
    case parent z of
        Just z2 -> case nextSiblingBranch z2 of
                       Just z3 -> Just z3
                       Nothing -> upNextSiblingBranch z2
        Nothing -> Nothing
