import Control.Monad.Trans.Class (lift)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Char (chr, ord)
import Data.Enumerator (Enumeratee, Iteratee(..), Step(..), Stream(..))
import qualified Data.Enumerator.List as EL (head, take)
import Data.Word (Word8, Word32)

utf8ToChar :: Monad m => Enumeratee Word8 Char m b
utf8ToChar (Continue k) = do
    mh <- EL.head
    case mh of
        Just h -> do
            tail <- EL.take (fromIntegral (lenUtf8 h - 1))
            newStep <- lift $ runIteratee $ k
                       $ Chunks [chr (decodeUtf8 (h:tail))]
            utf8ToChar newStep
        Nothing -> return $ Continue k
utf8ToChar step = return step

charToUtf8 :: Monad m => Enumeratee Char Word8 m b
charToUtf8 (Continue k) = do
    mc <- EL.head
    case mc of
        Just c -> do
            newStep <- lift $ runIteratee $ k $ Chunks (encodeUtf8 (ord c))
            charToUtf8 newStep
        Nothing -> return $ Continue k
charToUtf8 step = return step

encodeUtf8 :: Int -> [Word8]
encodeUtf8 x
    | x <=     0x7f = [ fromIntegral x ]
    | x <=    0x7ff = [ head 0xc0 (x `shiftR` 6)
                      , tail x
                      ]
    | x <=   0xffff = [ head 0xe0 (x `shiftR` 12)
                      , tail (x `shiftR` 6)
                      , tail x
                      ]
    | x <= 0x10ffff = [ head 0xf0 (x `shiftR` 18)
                      , tail (x `shiftR` 12)
                      , tail (x `shiftR` 6)
                      , tail x
                      ]
    | otherwise = error ("Illegal unicode: " ++ show x)
  where
    head :: Int -> Int -> Word8
    head prefix x = fromIntegral (prefix .|. x)
    tail :: Int -> Word8
    tail x = fromIntegral (0x80 .|. x .&. 0x3f)

lenUtf8 :: Word8 -> Int
lenUtf8 head
    | 0x80 .&. head == 0x00 = 1
    | 0xe0 .&. head == 0xc0 = 2
    | 0xf0 .&. head == 0xe0 = 3
    | 0xf8 .&. head == 0xf0 = 4
    | otherwise = error ("Illegal head byte: " ++ show head)

decodeUtf8 :: [Word8] -> Int
decodeUtf8 (x:xs) =
    case lenUtf8 x of
        1 -> fromIntegral x
        2 -> head 0x1f `shiftL` 6 .|. tail 0
        3 -> head 0x0f `shiftL` 12 .|. tail 0 `shiftL` 6 .|. tail 1
        4 -> head 0x07 `shiftL` 18 .|. tail 0 `shiftL` 12 .|. tail 1 `shiftL` 6
             .|. tail 2
  where
    head :: Int -> Int
    head mask = fromIntegral x .&. mask
    tail :: Int -> Int
    tail i = case fromIntegral (xs !! i) :: Int of
                 x | 0xc0 .&. x == 0x80 -> x .&. 0x3f
                   | otherwise -> error ("Illegal tail byte: " ++ show x)
