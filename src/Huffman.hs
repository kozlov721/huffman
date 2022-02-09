module Huffman where

import BitString                 (BitString)
import Control.Applicative.Tools ((<.>))
import Data.ByteString.Lazy      (ByteString)
import Data.List                 (sortOn)
import Data.Map                  (Map)
import Data.PQueue.Prio.Min      (MinPQueue)
import Data.Tuple                (swap)
import Data.Word                 (Word8)

import qualified BitString            as BS
import qualified Data.Bifunctor       as Bi
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as M
import qualified Data.PQueue.Prio.Min as Q


data CodeTree = Empty
              | Leaf Char
              | Node CodeTree CodeTree
              deriving Show

type CodeTable = Map Char [Word8]



countLetters :: String -> MinPQueue Int CodeTree
countLetters = Q.fromList
    . map (Bi.second Leaf . swap)
    . M.assocs
    . foldl updateMap M.empty
  where
    updateMap m k = M.insertWith (+) k 1 m


constructTree :: MinPQueue Int CodeTree -> CodeTree
constructTree q
    | Q.null q = Empty
    | Q.size q == 1 = lt
    | otherwise = constructTree
        $ Q.insert (lc + rc) (Node lt rt) q3
  where
    ((lc, lt), q2) = Q.deleteFindMin q
    ((rc, rt), q3) = Q.deleteFindMin q2


treeToTable :: CodeTree -> CodeTable
treeToTable (Leaf x) = M.singleton x [0]
treeToTable tree = go [] tree
  where
    go :: [Word8] -> CodeTree -> CodeTable
    go _ Empty = M.empty
    go code (Leaf x) = M.singleton x (reverse code)
    go code (Node left right) = M.union
        (go (0:code) left)
        (go (1:code) right)


prepareForEncoding :: String -> (CodeTree, CodeTable)
prepareForEncoding str = (tree, table)
  where
    tree = constructTree $ countLetters str
    table = treeToTable tree


encodeText :: String -> CodeTable -> ByteString
encodeText "" _ = BL.empty
encodeText str table = (\bs -> BS.toFullByte bs `BL.cons` BS.toByteString bs)
    $ foldr BS.cons BS.empty
    $ concatMap (table M.!) str


decodeText :: CodeTree -> ByteString -> Maybe String
decodeText Empty _ = Just ""
decodeText tree rbl = go tree tree =<< BS.fromByteStringPadded rbl
  where
    go :: CodeTree -> CodeTree -> BitString -> Maybe String
    go orig (Leaf c) bs
        | BS.null bs = Just [c]
        | otherwise = (c:) <$> go orig orig bs
    go orig (Node l r) bs
        | BS.null bs = Just []
        | b == 0 = go orig l rest
        | otherwise = go orig r rest
      where (b, rest) = BS.unconsUnsafe bs
    go _ _ _ = Nothing


encodeTree :: CodeTree -> ([Char], BitString)
encodeTree = Bi.second BS.fromBits . go
  where
    go :: CodeTree -> ([Char], [Word8])
    go Empty = ([], [])
    go (Leaf x) = ([x], [0])
    go (Node l r) = (newDataL ++ newDataR, 1 : newStructureL ++ newStructureR)
      where
        (newDataL, newStructureL) = go l
        (newDataR, newStructureR) = go r


decodeTree :: ([Char], BitString) -> Maybe CodeTree
decodeTree = fst <.> go
  where
    go :: ([Char], BitString) -> Maybe (CodeTree, ([Char], BitString))
    go (d:ds, bs) = do

        (b, rest)     <- BS.uncons bs
        (left, remL)  <- go (d:ds, rest)
        (right, remR) <- go remL

        if   b == 0
        then return (Leaf d, (ds, rest))
        else return (Node left right, remR)
    go _ = Nothing

