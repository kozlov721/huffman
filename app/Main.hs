module Main where

import Huffman
import Options.Applicative
import System.Exit         (exitFailure)
import System.IO           (hPutStrLn, stderr)
import Data.List.Extra     (takeEnd, dropEnd)

import qualified Data.ByteString.Lazy as BL

data Sample = Sample
    { decompress :: Bool
    , output     :: String
    , input      :: String
    }

sample :: Parser Sample
sample = Sample
    <$> switch (  long "decompress"
               <> short 'd'
               <> help "Whether to decompress"
               )
    <*> option str (  long "output"
                   <> short 'o'
                   <> value ""
                   <> metavar "STRING"
                   )
    <*> argument str (metavar "FILE")


compressFile :: FilePath -> FilePath -> IO ()
compressFile i o = readFile i >>= BL.writeFile o . encode

decompressFile :: FilePath -> FilePath -> IO ()
decompressFile i o = do
    dcmp <- decode <$> BL.readFile i
    case dcmp of
        Just str -> writeFile o str
        Nothing  -> hPutStrLn stderr "something went wrong" >> exitFailure

run :: Sample -> IO ()
run (Sample decode out inPath)
    | decode    = decompressFile inPath outPath
    | otherwise = compressFile inPath outPath
  where
    outPath
        | null out && decode =
            if   takeEnd 3 out == ".hf"
            then dropEnd 3 out
            else inPath ++ ".unhf"
        | null out && not decode = inPath ++ ".hf"
        | otherwise = out

main :: IO ()
main = execParser opts >>= run
  where
    opts = info
        (sample <**> helper)
        fullDesc

