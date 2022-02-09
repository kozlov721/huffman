import Data.Word (Word8)
import Data.Char (chr)
import Test.HUnit
import BitString (BitString)
import Huffman
import Control.Monad (void)
import qualified Data.ByteString.Lazy as BL
import qualified BitString as BS


tests = test $
    [ "cons-uncons" ~: show bits ~: bits ~=? bitsCycle bits
    | bits <- [toBinary n | n <- [0..100]]
    ]
    ++ concat
    [ [ "encode-decode-simple" ~: str ~: Just str ~=? encodeDecode str
      , "encode-decode-all"    ~: str ~: Just str ~=? (decodeAll . encodeAll) str
      ]
    | str <- [ "hello"
             , "world"
             , "a little longer string"
             , "non ǎščí 字母"
             , [chr x | x <- [1..127]]]
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
            let (b, ns) = BS.unconsUnsafe bs
            in b : bitStringToBits ns

encodeDecode :: String -> Maybe String
encodeDecode str = decodeText tree
    $ encodeText table str
  where
    (tree, table) = prepareForEncoding str

main :: IO ()
main = void $ runTestTT tests
