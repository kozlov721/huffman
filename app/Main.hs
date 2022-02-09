{-# LANGUAGE LambdaCase #-}
module Main where

import Huffman
import Options.Applicative
import System.Exit         (exitFailure)
import System.IO           (hPutStrLn, stderr)

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
    <*> option str (  long "input"
                   <> short 'i'
                   <> value ""
                   <> metavar "STRING"
                   )


compressFile :: FilePath -> FilePath -> IO ()
compressFile i o = readFile i >>= BL.writeFile o . encode

decompressFile :: FilePath -> FilePath -> IO ()
decompressFile i o = BL.readFile i
    >>= (\case Just str -> writeFile o str
               Nothing  -> hPutStrLn stderr "something went wrong")
    . decode

run :: Sample -> IO ()
run (Sample decode out inPath)
    | decode    = decompressFile inPath outPath
    | otherwise = compressFile inPath outPath
  where
    outPath
        | null out && decode =
            if take 4 (reverse inPath) == reverse ".cmp"
               then init
                  $ reverse
                  $ dropWhile (/='.')
                  $ reverse inPath
            else inPath ++ ".uncmp"
        | null out && not decode = inPath ++ ".cmp"
        | otherwise = out

main :: IO ()
main = execParser opts >>= run
  where
    opts = info
        (sample <**> helper)
        fullDesc

