
module BitString where


import Data.ByteString.Lazy (ByteString)
import Data.Maybe           (fromJust)
import Data.Word            (Word8)
import Control.Applicative.Tools ((<.>))
import Data.Bits            ((.&.))

import qualified Data.Bifunctor       as Bi
import qualified Data.ByteString.Lazy as BL


type Bit = Word8
type Byte = Word8
data BitString = BitString Byte Word8 ByteString
    deriving (Show)

uncons :: BitString -> Maybe (Bit, BitString)
uncons (BitString _ 0 t) = BL.uncons t
    >>= uncons . (\(h, t) -> BitString h 8 t)
uncons (BitString h l t) = Just
    (h `div` 2 ^ 7, BitString (h * 2) (l - 1) t)

cons :: Bit -> BitString -> BitString
cons b (BitString h 8 t) = cons b $ BitString 0 0 $ h `BL.cons` t
cons b (BitString h l t) = BitString (h `div` 2 + b * 2 ^ 7) (l + 1) t

unconsUnsafe :: BitString -> (Bit, BitString)
unconsUnsafe bs = case uncons bs of
    Nothing -> error "uncons on empty bit string"
    Just x  -> x

empty :: BitString
empty = BitString 0 0 BL.empty

singleton :: Word8 -> BitString
singleton = BitString 0 0 . BL.singleton

null :: BitString -> Bool
null (BitString _ 0 t) = BL.null t
null _ = False

fromByteString :: ByteString -> BitString
fromByteString = BitString 0 0

fromByteStringTrim :: Int -> ByteString -> BitString
fromByteStringTrim n = dropN n . fromByteString

fromByteStringPadded :: ByteString -> Maybe BitString
fromByteStringPadded = uncurry fromByteStringTrim
    <.> Bi.first fromIntegral
    <.> BL.uncons

fromBits :: [Word8] -> BitString
fromBits = foldr cons empty

toByteString :: BitString -> ByteString
toByteString (BitString h 0 t) = t
toByteString (BitString h 8 t) = h `BL.cons` t
toByteString bs = toByteString $ 0 `cons` bs

toByteStringPadded :: BitString -> ByteString
toByteStringPadded bs = toFullByte bs `BL.cons` toByteString bs

toFullByte :: BitString -> Word8
toFullByte (BitString _ l _) = 8 - l

dropN :: Int -> BitString -> BitString
dropN 0 bs = bs
dropN n bs
    | BitString.null bs = bs
    | otherwise = dropN (n - 1) $ snd $ unconsUnsafe bs
