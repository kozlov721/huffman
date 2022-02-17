{-# LANGUAGE TupleSections #-}

module Huffman
    ( encode
    , decode
    , encodeString
    , decodeString
    , prepareForEncoding
    , encodeTree
    , decodeTree
    )
      where

import Control.Applicative.Tools ((<.>))
import Data.BitString            (BitString)
import Data.ByteString.Lazy      (ByteString)
import Data.Int                  (Int64)
import Data.List                 (sortOn)
import Data.Map                  (Map)
import Data.PQueue.Prio.Min      (MinPQueue)
import Data.Tuple                (swap)
import Data.Word                 (Word8)

import qualified Data.Bifunctor            as Bi
import qualified Data.BitString            as BS
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Map                  as M
import qualified Data.PQueue.Prio.Min      as Q

data Tree = Empty
              | Leaf Char
              | Node Tree Tree
              deriving Show

type Table = Map Char [Word8]

toByteStringPadded :: BitString -> ByteString
toByteStringPadded = uncurry BL.cons . BS.toByteStringWithPadding

fromByteStringPadded :: ByteString -> Maybe BitString
fromByteStringPadded bl = uncurry BS.fromByteStringWithPadding <$> BL.uncons bl

countLetters :: String -> MinPQueue Int Tree
countLetters = Q.fromList
    . map (Bi.second Leaf . swap)
    . M.assocs
    . foldl updateMap M.empty
  where
    updateMap m k = M.insertWith (+) k 1 m

constructTree :: MinPQueue Int Tree -> Tree
constructTree q
    | Q.null q = Empty
    | Q.size q == 1 = lt
    | otherwise = constructTree
        $ Q.insert (lc + rc) (Node lt rt) q3
  where
    ((lc, lt), q2) = Q.deleteFindMin q
    ((rc, rt), q3) = Q.deleteFindMin q2

countNodes :: Tree -> Int
countNodes (Node l r) = 1 + countNodes l + countNodes r
countNodes _          = 0

treeToTable :: Tree -> Table
treeToTable (Leaf x) = M.singleton x [0]
treeToTable tree = go [] tree
  where
    go :: [Word8] -> Tree -> Table
    go _ Empty = M.empty
    go code (Leaf x) = M.singleton x (reverse code)
    go code (Node left right) = M.union
        (go (0:code) left)
        (go (1:code) right)

-- |\(\mathcal{O}(n \cdot \log n)\) Creates a Huffman Tree
-- and a code table from a 'String'.
prepareForEncoding :: String -> (Tree, Table)
prepareForEncoding str = (tree, table)
  where
    tree = constructTree $ countLetters str
    table = treeToTable tree

-- | \(\mathcal{O}(n)\) Encodes given 'String' using precomputed
-- code table.
encodeString :: Table -> String -> ByteString
encodeString _ "" = BL.empty
encodeString table str = toByteStringPadded
    $ foldr BS.cons BS.empty
    $ concatMap (table M.!) str

-- | \(\mathcal{O}(n)\) Decodes a 'String' usign precomputed
-- Huffman Tree.
decodeString :: Tree -> ByteString -> Maybe String
decodeString Empty _ = Just ""
decodeString tree rbl = go tree tree =<< fromByteStringPadded rbl
  where
    go :: Tree -> Tree -> BitString -> Maybe String
    go orig (Leaf c) bs
        | BS.null bs = Just [c]
        | otherwise = (c:) <$> go orig orig bs
    go orig (Node l r) bs
        | BS.null bs = Just []
        | otherwise = do
            (b, rest) <- BS.uncons bs
            if   b == 0
            then go orig l rest
            else go orig r rest
    go _ _ _ = Nothing

-- | Encodes a Huffman Tree using [succinct encoding](https://en.wikipedia.org/wiki/Binary_tree#Succinct_encodings)
-- for binary trees. The resulting 'ByteString' has a form of:
-- data | '\0' | length | structure
-- where length is the length is stored on 4 bytes and denotes on how many
-- bytes is stored the structure of the tree.
encodeTree :: Tree -> ByteString
encodeTree tree = BL.append ch
    $ BL.append (BL.singleton 0)
    $ BL.append l s
  where
    (ch, s) = Bi.bimap BLU.fromString (toByteStringPadded . BS.pack)
        $ go tree
    l = BL.pack
        [fromIntegral $ structLen `div` (256 ^ i) `mod` 256 | i <- [0..3]]
    structLen = (2 * countNodes tree + 1) `ceilDiv` 8
    ceilDiv a b = (a + b - 1) `div` b

    go :: Tree -> ([Char], [Word8])
    go Empty = ([], [])
    go (Leaf x) = ([x], [0])
    go (Node l r) = (newDataL ++ newDataR, 1 : newStructureL ++ newStructureR)
      where
        (newDataL, newStructureL) = go l
        (newDataR, newStructureR) = go r

-- | Recreates the Huffman Tree from data and structure.
decodeTree :: ([Char], BitString) -> Maybe Tree
decodeTree = fst <.> go
  where
    go :: ([Char], BitString) -> Maybe (Tree, ([Char], BitString))
    go (d:ds, bs) = do
        (b, rest)     <- BS.uncons bs
        if b == 0
        then return (Leaf d, (ds, rest))
        else do
            (left, remL)  <- go (d:ds, rest)
            (right, remR) <- go remL
            return (Node left right, remR)
    go _ = Nothing

-- | \(\mathcal{O}(n \cdot \log n)\) Encodes given 'Srting' using Huffman
-- code. Returns the encoded message prepended with encoded Huffman Tree.
encode :: String -> ByteString
encode str = BL.append encTree enc
  where
    (tree, table) = prepareForEncoding str
    enc = encodeString table str
    encTree = encodeTree tree

splitByteString :: ByteString -> Maybe (ByteString, ByteString)
splitByteString = go BL.empty
  where
    go :: ByteString -> ByteString -> Maybe (ByteString, ByteString)
    go pref rem = do
        (h, t) <- BL.uncons rem
        if   h == 0
        then return (pref, t)
        else go (h `BL.cons` pref) t

-- | \(\mathcal{O}(n)\) Decodes a 'ByteString' into the original message.
-- Returns 'Nothing' in case of failure.
decode :: ByteString -> Maybe String
decode bl = do
    (d, t) <- Bi.first (BLU.toString . BL.reverse) <$> splitByteString bl
    let (l, t2)   = Bi.first getSLen $ BL.splitAt 4 t
    let (s, text) = Bi.first fromByteStringPadded $ BL.splitAt (l + 1) t2
    tree <- decodeTree . (d,) =<< s
    decodeString tree text

getSLen :: ByteString -> Int64
getSLen = foldr ( (\x y -> y * 256 + x)
                . (\x -> fromIntegral x :: Int64)
                ) 0 . BL.unpack

