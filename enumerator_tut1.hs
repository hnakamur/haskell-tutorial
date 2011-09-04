import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (toLower)
import Data.Enumerator
    (($$), (>>==), Enumeratee, Enumerator(..), Iteratee(..), Step(..),
     Stream(..), continue, enumList, joinI, returnI, run, run_, yield)
import Data.Word (Word8)
{-import qualified Data.Enumerator as E (map)-}
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL (consume)
import Data.Enumerator.List (fold, head)
import Numeric (readHex, showHex)
import Prelude hiding (head)
import System.IO (Handle, IOMode(..), hGetLine, hIsEOF, isEOF, withFile)
import Test.QuickCheck

sum6 :: Monad m => Iteratee Int m Int
sum6 = do
    maybeNum <- head
    case maybeNum of
        Nothing -> return 0
        Just i -> do
            rest <- sum6
            return $ i + rest

interleaved :: MonadIO m => Iteratee String m ()
interleaved = do
    maybeLine <- head
    case maybeLine of
        Nothing -> return ()
        Just line -> do
            liftIO $ putStrLn line
            interleaved

head' :: Monad m => Iteratee a m (Maybe a)
head' =
    continue go
  where
    go :: Monad m => Stream a -> Iteratee a m (Maybe a)
    go (Chunks []) = continue go
    go (Chunks (x:xs)) = yield (Just x) (Chunks xs)
    go EOF = yield Nothing EOF


{-head :: Monad m => Iteratee a m (Maybe a)
head = liftI step where
	step (Chunks []) = Continue $ returnI . step
	step (Chunks (x:xs)) = Yield (Just x) (Chunks xs)
	step EOF = Yield Nothing EOF-}
-- | As 'liftFoldL', but strict in its accumulator.
-- liftFoldL' :: Monad m => (b -> a -> b) -> b -> Iteratee a m b
-- liftFoldL' f = liftI . step where
-- 	fold = DataList.foldl' f
-- 	step acc chunk = case chunk of
-- 		Chunks [] -> Continue $ returnI . step acc
-- 		Chunks xs -> Continue $ returnI . (step $! fold acc xs)
-- 		EOF -> Yield acc EOF

{-sum6' :: Monad m => Iteratee Int m Int
sum6' = liftFoldL' (+) 0-}
sum6' :: Monad m => Iteratee Int m Int
sum6' = fold (+) 0

{-data Stream a
	= Chunks [a]
	| EOF
	deriving (Show, Eq)-}
{-
data Step a m b
	-- | The 'Iteratee' is capable of accepting more input. Note that more input
	-- is not necessarily required; the 'Iteratee' might be able to generate a
	-- value immediately if it receives 'EOF'.
	= Continue (Stream a -> Iteratee a m b)
	--
	-- | The 'Iteratee' cannot receive any more input, and has generated a
	-- result. Included in this value is left-over input, which can be passed to
	-- composed 'Iteratee's.
	| Yield b (Stream a)
	| Error Exc.SomeException
-}
{-newtype Iteratee a m b = Iteratee { runIteratee :: m (Step a m b) }-}

{-consume :: Monad m => Iteratee a m [a]
consume = do
    h <- head
    case h of
        Nothing -> return []
        Just x -> do
            rest <- consume
            return (x:rest)-}

{-consume :: Monad m => Iteratee a m [a]
consume =
    continue $ go []
  where
    go :: Monad m => [a] -> Stream a -> Iteratee a m [a]
    go xs (Chunks []) = continue $ go xs
    go xs (Chunks ys) = continue $ go (xs ++ ys)
    go xs EOF = yield xs EOF-}

consume :: Monad m => Iteratee a m [a]
consume =
    continue $ go id
  where
    go :: Monad m => ([a] -> [a]) -> Stream a -> Iteratee a m [a]
    go f (Chunks []) = continue $ go f
    go f (Chunks xs) = continue $ go (f . (xs ++))
    go f EOF = yield (f []) EOF

consumeInterleaved :: Monad m => Iteratee a m [a]
consumeInterleaved = do
    h <- head
    _ <- head
    case h of
        Nothing -> return []
        Just x -> do
            rest <- consumeInterleaved
            return (x:rest)

consumeInterleaved' :: Monad m => Iteratee a m [a]
consumeInterleaved' =
    continue $ go id
  where
    go :: Monad m => ([a] -> [a]) -> Stream a -> Iteratee a m [a]
    go f (Chunks []) = continue $ go f
    go f (Chunks xs) = continue $ go (f . (xs ++))
    go f EOF = yield (everyOther (f [])) EOF

everyOther [] = []
everyOther [x] = [x]
everyOther (x:y:xs) = x : everyOther xs

sum7 :: Monad m => Iteratee Int m Int
sum7 = Iteratee $ do
    Continue k <- runIteratee sum6
    runIteratee $ k $ Chunks [1..10]

sum8 :: Monad m => Iteratee Int m Int
sum8 = Iteratee $ do
    step <- runIteratee sum6
    case step of
        Continue k -> runIteratee $ k $ Chunks [1..10]
        _ -> return step

sum9 :: Monad m => Iteratee Int m Int -> Iteratee Int m Int
sum9 orig = Iteratee $ do
    step <- runIteratee orig
    case step of
        Continue k -> runIteratee $ k $ Chunks [1..10]
        _ -> return step

sum10 :: Monad m => Step Int m Int -> Iteratee Int m Int
sum10 (Continue k) = k $ Chunks [1..10]
sum10 step = returnI step

applyEnum :: Monad m => Enumerator a m b -> Iteratee a m b -> Iteratee a m b
applyEnum enum iter = Iteratee $ do
    step <- runIteratee iter
    runIteratee $ enum step

sum11 :: Monad m => Enumerator Int m Int
sum11 (Continue k) = k $ Chunks [11..20]
sum11 step = returnI step

{-getNumberEnum :: MonadIO m => Enumerator Int m b
getNumberEnum (Continue k) = do
    x <- liftIO getLine
    if x == "q"
        then continue k
        else k (Chunks [read x]) >>== getNumberEnum
getNumberEnum step = returnI step-}

{-getNumberEnum :: MonadIO m => Enumerator Int m b
getNumberEnum (Continue k) = do
    eof <- liftIO isEOF
    if eof
        then continue k
        else do
            x <- liftIO getLine
            case maybeRead x :: Maybe Int of
                (Just x') -> k (Chunks [x']) >>== getNumberEnum
                Nothing -> fail "number expected"
getNumberEnum step = returnI step-}

hGetNumberEnum :: MonadIO m => Handle -> Enumerator Int m b
hGetNumberEnum handle (Continue k) = do
    eof <- liftIO (hIsEOF handle)
    if eof
        then continue k
        else do
            x <- liftIO (hGetLine handle)
            case maybeRead x :: Maybe Int of
                (Just x') -> k (Chunks [x']) >>== hGetNumberEnum handle
                Nothing -> fail "number expected"
hGetNumberEnum _ step = returnI step

runWithTwoFiles f1 f2 =
    withFile f1 ReadMode (\h1 ->
    withFile f2 ReadMode (\h2 ->
        run_ (hGetNumberEnum h1 $$ hGetNumberEnum h2 $$ sum6) >>= print))

main = runWithTwoFiles "foo.txt" "bar.txt"

{-type Enumerator a m b = Step a m b -> Iteratee a m b-}

intsToStrings :: (Show a, Monad m) => Iteratee a m String
intsToStrings = (unlines . map show) `fmap` consume

getStringEnum :: MonadIO m => Enumerator String m b
getStringEnum (Continue k) = do
    x <- liftIO getLine
    if x == ""
        then continue k
        else k (Chunks [x]) >>== getStringEnum
getStringEnum step = returnI step

printStrings :: Iteratee String IO ()
printStrings = do
    mstring <- head
    case mstring of
        Nothing -> return ()
        Just string -> do
            liftIO $ putStrLn string
            printStrings

getWordEnum :: MonadIO m => Enumerator String m b
getWordEnum (Continue k) = do
    x <- liftIO getLine
    if x == ""
        then continue k
        else k (Chunks (words x)) >>== getWordEnum
getWordEnum step = returnI step

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                [(x,"")] -> Just x
                _ -> Nothing

lineEnum :: MonadIO m => Enumerator String m b
lineEnum (Continue k) = do
    x <- liftIO getLine
    if x == "q"
        then continue k
        else k (Chunks [x]) >>== lineEnum
lineEnum step = returnI step

sumIter :: Monad m => Iteratee Int m Int
sumIter = do
    maybeNum <- head
    case maybeNum of
        Nothing -> return 0
        Just i -> do
            rest <- sumIter
            return $ i + rest

sumIterString :: Monad m => Iteratee String m Int
sumIterString = Iteratee $ do
    innerStep <- runIteratee sumIter
    return $ go innerStep
  where
    go :: Monad m => Step Int m Int -> Step String m Int
    go (Yield res _) = Yield res EOF
    go (Error err) = Error err
    go (Continue k) = Continue $ \strings -> Iteratee $ do
        let ints = fmap read strings :: Stream Int
        step <- runIteratee $ k ints
        return $ go step

mapIter :: Monad m => (aOut -> aIn) -> Iteratee aIn m b -> Iteratee aOut m b
mapIter f innerIter = Iteratee $ do
    innerStep <- runIteratee innerIter
    return $ go innerStep
  where
    go (Yield res _) = Yield res EOF
    go (Error err) = Error err
    go (Continue k) = Continue $ \strings -> Iteratee $ do
        let ints = fmap f strings
        step <- runIteratee $ k ints
        return $ go step

{-type Enumeratee aOut aIn m b = Step aIn m b -> Iteratee aOut m (Step aIn m b)-}

skip :: Monad m => Enumeratee a a m b
skip (Continue k) = do
    x <- head
    _ <- head -- 1‚Â“Ç‚Ý”ò‚Î‚·
    case x of
        Nothing -> return $ Continue k
        Just y -> do
            newStep <- lift $ runIteratee $ k $ Chunks [y]
            skip newStep
skip step = return step

maybeReadHex :: Num a => String -> Maybe a
maybeReadHex s = case readHex s of
                     [(x,"")] -> Just x
                     _ -> Nothing

enumReadHex :: Monad m => Enumeratee Char Word8 m b
enumReadHex (Continue k) = do
    mh <- head
    case mh of
        Just h -> do
            ml <- head
            case ml of
                Just l ->
                    case maybeReadHex [h,l] :: Maybe Word8 of
                        Nothing -> fail "hex string expected"
                        Just x -> do
                            newStep <- lift $ runIteratee $ k $ Chunks [x]
                            enumReadHex newStep
                Nothing -> fail "hex char expected"
        Nothing -> return $ Continue k
enumReadHex step = return step

enumShowHex :: Monad m => Enumeratee Word8 Char m b
enumShowHex (Continue k) = do
    mw <- head
    case mw of
        Just w -> do
            newStep <- lift $ runIteratee $ k $ Chunks (showHex w "")
            enumShowHex newStep
        Nothing -> return $ Continue k
enumShowHex step = return step

{-run_ (enumList 2 "DEADBEEF" $$ joinI $ enumReadHex $$ EL.consume) >>= print
run_ (enumList 2 [222,173,190,239] $$ joinI $ enumShowHex $$ EL.consume) >>= print-}

prop_idempotent :: [Char] -> Bool
prop_idempotent xs = result == Prelude.map toLower xs
  where
    result = do
        ys <- run_ (enumList 1 xs $$ joinI $ enumReadHex $$
                    joinI $ enumShowHex $$ EL.consume)
        ys
