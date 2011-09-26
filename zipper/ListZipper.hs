type ListZipper a = ([a], [a])

fromList :: [a] -> ListZipper a
fromList xs = (xs, [])

goForward :: ListZipper a -> Maybe (ListZipper a)
goForward ([], _) = Nothing
goForward (x:xs, bs) = Just (xs, x:bs)

goBackward :: ListZipper a -> Maybe (ListZipper a)
goBackward (_, []) = Nothing
goBackward (xs, b:bs) = Just (b:xs, bs)

goHead :: ListZipper a -> Maybe (ListZipper a)
goHead (xs, []) = Just (xs, [])
goHead (xs, bs) = goBackward (xs, bs) >>= goHead

insert :: a -> ListZipper a -> Maybe (ListZipper a)
insert x (xs, bs) = Just (x:xs, bs)

delete :: ListZipper a -> Maybe (ListZipper a)
delete ([], _) = Nothing
delete (x:xs, bs) = Just (xs, bs)

toList :: ListZipper a -> [a]
toList (xs, bs) =
    case goHead (xs, bs) of
        Just (ys, []) -> ys
