import Control.Arrow ((>>>), Arrow, arr, first)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

second :: Arrow a => a b c -> a (d, b) (d, c)
second f = arr swap >>> first f >>> arr swap

(***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')
(***) f g = first f >>> second g

clone :: a -> (a, a)
clone a = (a, a)

(&&&) :: Arrow a => a b c -> a b c' -> a b (c, c')
(&&&) f g = arr clone >>> f *** g

addA :: (Arrow cat, Num c) => cat a c -> cat a c -> cat a c
{-addA f g = f &&& g >>> arr (\(y, z) -> y + z)-}
addA f g = arr clone >>> f *** g >>> arr (\(y, z) -> y + z)
