data Term
  = Var String
  | Lambda String Term
  | App Term Term
  | If Term Term Term
  deriving (Show)

type TermZipper = (Term, TermContext)
data TermContext
  = Root
  | Lambda1 String TermContext
  | App1 TermContext Term
  | App2 Term TermContext
  | If1 TermContext Term Term
  | If2 Term TermContext Term
  | If3 Term Term TermContext
  deriving (Show)

fromZipper :: TermZipper -> Term
fromZipper z = f z
  where
    f :: TermZipper -> Term
    f (t1, Root) = t1
    f (t1, Lambda1 s c) = f (Lambda s t1, c)
    f (t1, App1 c t2) = f (App t1 t2, c)
    f (t2, App2 t1 c) = f (App t1 t2, c)
    f (t1, If1 c t2 t3) = f (If t1 t2 t3, c)
    f (t2, If2 t1 c t3) = f (If t1 t2 t3, c)
    f (t3, If3 t1 t2 c) = f (If t1 t2 t3, c)

toZipper :: Term -> TermZipper
toZipper t = (t, Root)

getHole :: TermZipper -> Term
getHole (t, _) = t

setHole :: Term -> TermZipper -> TermZipper
setHole h (_, c) = (h, c)

down :: TermZipper -> Maybe TermZipper
down (Var s, c) = Nothing
down (Lambda s t1, c) = Just (t1, Lambda1 s c)
down (App t1 t2, c) = Just (t1, App1 c t2)
down (If t1 t2 t3, c) = Just (t1, If1 c t2 t3)

up :: TermZipper -> Maybe TermZipper
up (t1, Root) = Nothing
up (t1, Lambda1 s c) = Just (Lambda s t1, c)
up (t1, App1 c t2) = Just (App t1 t2, c)
up (t2, App2 t1 c) = Just (App t1 t2, c)
up (t1, If1 c t2 t3) = Just (If t1 t2 t3, c)
up (t2, If2 t1 c t3) = Just (If t1 t2 t3, c)
up (t3, If3 t1 t2 c) = Just (If t1 t2 t3, c)

left :: TermZipper -> Maybe TermZipper
left (t1, Root) = Nothing
left (t1, Lambda1 s c) = Nothing
left (t1, App1 c t2) = Nothing
left (t2, App2 t1 c) = Just (t1, App1 c t2)
left (t1, If1 c t2 t3) = Nothing
left (t2, If2 t1 c t3) = Just (t1, If1 c t2 t3)
left (t3, If3 t1 t2 c) = Just (t2, If2 t1 c t3)

right :: TermZipper -> Maybe TermZipper
right (t1, Root) = Nothing
right (t1, Lambda1 s c) = Nothing
right (t1, App1 c t2) = Just (t2, App2 t1 c)
right (t2, App2 t1 c) = Nothing
right (t1, If1 c t2 t3) = Just (t2, If2 t1 c t3)
right (t2, If2 t1 c t3) = Just (t3, If3 t1 t2 c)
right (t3, If3 t1 t2 c) = Nothing

top :: TermZipper -> Maybe TermZipper
top (t, Root) = Just (t, Root)
top (t, c) = up (t, c) >>= top

fac = Lambda "n"
  (If (App (App (Var "=") (Var "n")) (Var "0"))
      (Var "1")
      (App (App (Var "+") (Var "n"))
           (App (Var "fac")
                (App (Var "pred") (Var "n")))))
