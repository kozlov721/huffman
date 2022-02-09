import BitString        (BitString)
import Control.Monad    (void)
import Data.Char        (chr)
import Data.Maybe       (isJust, fromJust)
import Data.Word        (Word8)
import Huffman
import System.Directory (listDirectory)
import System.Exit
import System.Process   (readProcessWithExitCode)
import Test.HUnit

import qualified BitString            as BS
import qualified Data.ByteString.Lazy as BL


tests = test $
    [ "cons-uncons" ~: show bits ~: bits ~=? bitsCycle bits
    | bits <- [toBinary n | n <- [0..100]]
    ]
    ++ concat
    [ [ "encode-decode-simple" ~: str ~: Just str ~=? encodeDecode str
      , "encode-decode-all"    ~: str ~: Just str ~=? (decode . encode) str
      ]
    | str <- [ "hello"
             , "world"
             , "a little longer string"
             , "non ǎščí 字母"
             , [chr x | x <- [1..127]]]
    ]
    ++
    [ "comp-decomp-file" ~: path ~: encodeDecodeFile path
    | path <- map ("./resources/"++) ["small.txt", "medium.txt"]
    ]

bitsCycle :: [Word8] -> [Word8]
bitsCycle = bitStringToBits . foldr BS.cons BS.empty

toBinary :: Word8 -> [Word8]
toBinary 0 = []
toBinary n = (n `mod` 2) : toBinary (n `div` 2)

bitStringToBits :: BitString -> [Word8]
bitStringToBits = go
  where
    go :: BitString -> [Word8]
    go bs
        | BS.null bs = []
        | otherwise =
            let (b, ns) = fromJust $ BS.uncons bs
            in  b : bitStringToBits ns

encodeDecode :: String -> Maybe String
encodeDecode str = decodeText tree
    $ encodeText table str
  where
    (tree, table) = prepareForEncoding str

encodeDecodeFile :: FilePath -> IO Bool
encodeDecodeFile path = isJust . encodeDecode
    <$> readFile path

main :: IO ()
main =  void $ runTestTT tests

