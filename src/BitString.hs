module BitString
    ( BitString
    , Bit
    , uncons
    , cons
    , BitString.null
    , empty
    , singleton
    , fromByteString
    , fromBits
    , fromByteStringWithPadding
    , fromByteStringPadded
    , toByteString
    , toByteStringWithPadding
    , toByteStringPadded
    , dropN
    , dropNSafe
    ) where


import Data.ByteString.Lazy (ByteString)
import Data.Maybe           (fromJust)
import Data.Word            (Word8)
import Data.Int             (Int64)

import qualified Data.Bifunctor       as Bi
import qualified Data.ByteString.Lazy as BL


-- | Alias for 'Word8'. /Be cautious to only use/
-- /@0@ and @1@ as values, otherwise the correctness is/
-- /not guaranteed!/
type Bit = Word8

-- | Wrapper around lazy 'ByteString'
-- which allows constructing the 'ByteString'
-- from bits instead of bytes. This can be a useful
-- abstraction when constructing binary data.
data BitString = BitString
    Word8      -- ^ head
    Word8      -- ^ number of used bits in the head
    ByteString -- ^ tail
    deriving (Show)

-- | /O(1)/ Returns the head and tail of a 'BitString', or 'Nothing' if empty.
uncons :: BitString -> Maybe (Bit, BitString)
uncons (BitString _ 0 t) = do
    (h, t) <- BL.uncons t
    uncons $ BitString h 8 t
uncons (BitString h l t) = Just
    (h `div` 2 ^ 7, BitString (h * 2) (l - 1) t)

-- | /O(1)/ 'cons' is analogous to '(Prelude.:)' for lists.
cons :: Bit -> BitString -> BitString
cons b (BitString h 8 t) = cons b $ BitString 0 0 $ h `BL.cons` t
cons b (BitString h l t) = BitString (h `div` 2 + b * 2 ^ 7) (l + 1) t

-- | /O(1)/ checks whether `BitString` is empty.
null :: BitString -> Bool
null (BitString _ 0 t) = BL.null t
null _                 = False

-- | /O(1)/ constructs an empty 'BitString'.
empty :: BitString
empty = BitString 0 0 BL.empty

-- | /O(1)/ converts one bit into a `BitString`.
singleton :: Bit -> BitString
singleton = BitString 0 0 . BL.singleton

-- | /O(1)/ converts a lazy 'ByteString' into a 'BitString'.
fromByteString :: ByteString -> BitString
fromByteString = BitString 0 0

-- | /O(c)/ reverse of 'toByteStringWithPadding'. Returns 'Nothing'
-- in case the provided padding is greater than the size of the 'BitString'.
fromByteStringWithPadding :: Word8 -> ByteString -> Maybe BitString
fromByteStringWithPadding n bl = dropN (fromIntegral n) $ fromByteString bl

-- | /O(c)/ reverse of 'toByteStringPadded'. Returns 'Nothing'
-- in case the padding denoted by the first byte
-- is greater than the size of the 'BitString'.
fromByteStringPadded :: ByteString -> Maybe BitString
fromByteStringPadded bl = BL.uncons bl >>= uncurry fromByteStringWithPadding

-- | /O(n)/ constructs a 'BitString' from a list of bits.
fromBits :: [Bit] -> BitString
fromBits = foldr cons empty

-- | /O(1)/ converts a `BitString` back to `ByteString`.
-- The `BitString` is padded with zeros if its length is not divisible by 8.
toByteString :: BitString -> ByteString
toByteString (BitString h 0 t) = t
toByteString (BitString h 8 t) = h `BL.cons` t
toByteString bs                = toByteString $ 0 `cons` bs

-- | /O(1)/ similar to 'toByteString', but also returns the
-- number of leading zeros.
toByteStringWithPadding :: BitString -> (Word8, ByteString)
toByteStringWithPadding bs@(BitString _ l _) = (8 - l, toByteString bs)

-- | /O(1)/ similar to 'toByteStringWithPadding', but prepends the final
-- 'ByteString' with the number of leading zeros.
--
-- Equivalent to:
--
-- > uncurry ByteString.cons . toByteStringWithPadding
toByteStringPadded :: BitString -> ByteString
toByteStringPadded = uncurry BL.cons . toByteStringWithPadding

-- | /O(n)/ drops first n bits. Fails if n is greater than
-- the size of the 'BitString'.
dropN :: Int64 -> BitString -> Maybe BitString
dropN 0 bs = Just bs
dropN n bs = dropN (n - 1) . snd =<< uncons bs

-- | /O(n)/ similar to 'dropN' but return empty 'BitString'
-- instead of 'Nothing'.
dropNSafe :: Int64 -> BitString -> BitString
dropNSafe n bs
    | n == 0 || BitString.null bs = bs
    | otherwise = dropNSafe (n - 1) $ snd $ fromJust $ uncons bs

