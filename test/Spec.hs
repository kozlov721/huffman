import Data.Word (Word8)
import Test.QuickCheck
import BitString (BitString)
import Huffman
import qualified Data.ByteString.Lazy as BL
import qualified BitString as BS

genBits :: Gen [Word8]
genBits = do
    n <- chooseInt (0, 16)
    sequenceA [choose (0, 1) | _ <- [0..n]]


prop_consUnconsConsistency :: [Word8] -> Bool
prop_consUnconsConsistency x = bitsCycle x == x

prop_byteStringConsistency :: [Word8] -> Bool
prop_byteStringConsistency x = bitStringToBits bs == bits
  where
    bits = concatMap wordToBits x
    bs = BS.fromByteString $ BL.pack x


bitsCycle :: [Word8] -> [Word8]
bitsCycle = bitStringToBits . foldr BS.cons BS.empty

wordToBits :: Word8 -> [Word8]
wordToBits n = reverse $ go n 8
  where
    go :: Word8 -> Word8 -> [Word8]
    go _ 0 = []
    go n d = fromIntegral (n `mod` 2) : go (n `div` 2) (d - 1)

bitStringToBits :: BitString -> [Word8]
bitStringToBits = go
  where
    go :: BitString -> [Word8]
    go bs
        | BS.null bs = []
        | otherwise =
            let (b, ns) = BS.unconsUnsafe bs
            in b : bitStringToBits ns

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
    bitres  <- quickCheckResult $ forAll genBits prop_consUnconsConsistency
    case bitres of
        Failure {failingTestCase = fail} -> failureInfo fail (bitsCycle . read)
        _ -> putStrLn "BitString OK"
    quickCheck prop_byteStringConsistency
    quickCheck (\x -> let bl = BL.pack x in bl == BS.toByteString (BS.fromByteString bl))
    huffres <- quickCheckResult prop_Huffman
    case huffres of
        Failure {failingTestCase = fail} -> failureInfo fail encodeDecode
        _ -> putStrLn "Huffman string consistency OK"
