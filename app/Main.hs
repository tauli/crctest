{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Digest.CRC32 (crc32)
import Data.Word (Word32)
import System.Console.ANSI
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO (openFile, IOMode(..))
import Text.Printf (printf)
import Text.Regex.PCRE.Heavy

main :: IO ()
main = do
  args <- getArgs
  case args of
    (f:fs)    -> hashDir args
    otherwise -> putStrLn "please provide File Name"

hashDir :: [FilePath] -> IO ()
hashDir fs =
  mapM_ (\f -> do
    t <- doesFileExist f
    if t then checkFile f else pure ()
  ) fs

checkFile :: FilePath -> IO ()
checkFile fName = do
  h   <- openFile fName ReadMode
  crc <- BL.readFile fName >>= return . toHex . crc32
  case findHash fName of
    (fPre:fHash:fPost:_) -> do
      pFileName fPre fHash fPost
      putStr " "
      if crc == fHash then pOk else pFail
      putStrLn ""
    _ -> putStrLn fName

toHex :: Word32 -> String
toHex = printf "%08X"

findHash :: String -> [String]
findHash fName = case scan [re|(.*)([\dABCDEF]{8})(.*)|] fName of
                   ((_,l):_) ->  l
                   _         -> []

pOk :: IO ()
pOk = do
  setSGR [SetColor Foreground Dull Green]
  putStr "\x2714"
  setSGR []

pFail :: IO ()
pFail = do
  setSGR [SetColor Foreground Dull Red]
  putStr "\x2716"
  setSGR []

pFileName :: String -> String -> String -> IO ()
pFileName fPre fHash fPost = do
  putStr fPre
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr fHash
  setSGR []
  putStr fPost
