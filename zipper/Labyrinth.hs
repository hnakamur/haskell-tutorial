data Node a
    = DeadEnd a
    | Passage a (Node a)
    | Fork a (Node a) (Node a)
    deriving (Show)

get :: Node a -> a
get (DeadEnd x) = x
get (Passage x _) = x
get (Fork x _ _) = x

put :: a -> Node a -> Node a
put x (DeadEnd _) = DeadEnd x
put x (Passage _ n) = Passage x n
put x (Fork _ l r) = Fork x l r

l1 = Fork (0,2)
         (Passage (2,0)
             (Fork (1,0)
                 (DeadEnd (0,-1))
                 (Passage (0,1) (DeadEnd (0,0)))))
         (Fork (-2,0) (DeadEnd (-1,0)) (DeadEnd (0,-2)))

data Branch a
    = KeepStraightOn a
    | TurnLeft a (Node a)
    | TurnRight a (Node a)
    deriving (Show)
type Thread a = [Branch a]
type Zipper a = (Thread a, Node a)

turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (t, Fork x l r) = Just (TurnRight x l : t, r)
turnRight _ = Nothing

turnLeft :: Zipper a -> Maybe (Zipper a)
turnLeft (t, Fork x l r) = Just (TurnLeft x r : t, l)
turnLeft _ = Nothing

keepStraightOn :: Zipper a -> Maybe (Zipper a)
keepStraightOn (t, Passage x n) = Just (KeepStraightOn x : t, n)
keepStraightOn _ = Nothing

back :: Zipper a -> Maybe (Zipper a)
back ([], _) = Nothing
back (KeepStraightOn x : t, n) = Just (t, Passage x n)
back (TurnLeft x r : t, l) = Just (t, Fork x l r)
back (TurnRight x l : t, r) = Just (t, Fork x l r)

{-retrieve :: Thread -> Node a -> a
retrieve [] n = get n
retrieve (KeepStraightOn:bs) (Passage _ n) = retrieve bs n
retrieve (TurnLeft:bs) (Fork _ l _) = retrieve bs l
retrieve (TurnRight:bs) (Fork _ _ r) = retrieve bs r

update :: (a -> a) -> Thread -> Node a -> a
update f t n = f (retrieve t n)-}
