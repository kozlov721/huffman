{-# LANGUAGE TupleSections #-}
module BitString where


import Data.ByteString.Lazy (ByteString)
import Data.Maybe           (fromJust)
import Data.Word            (Word8)

import qualified Data.Bifunctor       as BI
import qualified Data.ByteString.Lazy as BL


type Bit = Word8
type Byte = Word8
type BitString = ((Byte, Word8), ByteString)
-- data BitString' = BitString Word8 Word8 ByteString

unconsSafe :: BitString -> Maybe (Bit, BitString)
unconsSafe ((_, 0), bs) = unconsSafe . BI.first (,8) =<< BL.uncons bs
unconsSafe ((b, n), bs) = Just (b `div` 2 ^ (n - 1) `mod` 2, ((b, n - 1), bs))

uncons :: BitString -> (Bit, BitString)
uncons bs = case unconsSafe bs of
    Nothing -> error "uncons on empty BitString"
    Just x  -> x

cons :: Bit -> BitString -> BitString
cons b ((h, 8), bs) = ((b, 1), h `BL.cons` bs)
cons b ((h, n), bs) = ((h + b * 2 ^ n, n + 1), bs)

null :: BitString -> Bool
null ((_, 0), x) = BL.null x
null _           = False

empty :: BitString
empty = ((0, 0), BL.empty)

fromByteString :: ByteString -> BitString
fromByteString bStr
    | BL.null bStr = ((0, 8), BL.empty)
    | otherwise = ((BL.head bStr, 0), BL.tail bStr)

toByteString :: BitString -> ByteString
toByteString ((h, 8), bs) = h `BL.cons` bs
toByteString bs           = toByteString $ 0 `cons` bs

pad :: Bit -> Word8 -> BitString -> (BitString, Word8)
pad _ n bs@((_, 8), _) = (bs, n)
pad b n bs             = pad b (n + 1) (b `cons` bs)

padZeros :: BitString -> (BitString, Word8)
padZeros = pad 0 0

padOnes :: BitString -> (BitString, Word8)
padOnes = pad 1 0

removePadding :: Word8 -> BitString -> BitString
removePadding 0 = id
removePadding n = removePadding (n - 1) . snd . uncons
