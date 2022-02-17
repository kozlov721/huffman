import Control.Monad  (void)
import Data.BitString (BitString)
import Data.Char      (chr)
import Data.Maybe     (fromJust, isJust)
import Data.Word      (Word8)
import Huffman
import System.Exit
import Test.HUnit

import qualified Data.BitString       as BS
import qualified Data.ByteString.Lazy as BL


tests = test $ concat
    [ [ "encode-decode-simple" ~: str ~: Just str ~=? encodeDecode str
      , "encode-decode-all"    ~: str ~: Just str ~=? (decode . encode) str
      ]
    | str <- [ "hello"
             , "world"
             , "a little longer string"
             , "non ǎščí 字母"
             ]
    ]
    ++
    [ "comp-decomp-file" ~: path ~: encodeDecodeFile path
    | path <- map ("./resources/"++) [ "small.txt"
                                     , "medium.txt"
                                     , "non-ascii.txt"
                                     ]
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
encodeDecode str = decodeString tree
    $ encodeString table str
  where
    (tree, table) = prepareForEncoding str

encodeDecodeFile :: FilePath -> IO Bool
encodeDecodeFile path = isJust . encodeDecode
    <$> readFile path

main :: IO ()
main =  void $ runTestTT tests

