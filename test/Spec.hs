import Data.Word (Word8)
import Test.QuickCheck
import BitString (BitString)
import Huffman

import qualified BitString as BS

genBits :: Gen [Word8]
genBits = do
    n <- chooseInt (0, 100)
    sequenceA [choose (0, 1) | _ <- [0..n]]


prop_BitString :: [Word8] -> Bool
prop_BitString x = bitsCycle x == x


toBits :: BitString -> [Word8]
toBits bs
    | BS.null bs = []
    | otherwise =
        let (b, ns) = BS.uncons bs
        in b : toBits ns


bitsCycle :: [Word8] -> [Word8]
bitsCycle = toBits . foldr BS.cons BS.empty


failureInfo :: (Show b) => [String] -> (String -> b) -> IO ()
failureInfo [] _ = return ()
failureInfo (x:xs) f = printOne x >> failureInfo xs f
  where
    printOne str = putStr "Expected: "
        >>  putStrLn x
        >>  putStr "Received: "
        >>  print (f str)


encodeDecode :: String -> Maybe String
encodeDecode str = decodeText tree
    $ encodeText str table
  where
    (tree, table) = prepareForEncoding str


prop_Huffman :: String -> Bool
prop_Huffman str = case encodeDecode str of
    Nothing -> False
    Just x  -> str == x


main :: IO ()
main = do
    bitres  <- quickCheckResult $ forAll genBits prop_BitString
    huffres <- quickCheckResult prop_Huffman
    case bitres of
        Failure {failingTestCase = fail} -> failureInfo fail (bitsCycle . read)
        _ -> putStrLn "BitString OK"
    case huffres of
        Failure {failingTestCase = fail} -> failureInfo fail encodeDecode
        _ -> putStrLn "Huffman string consistency OK"
