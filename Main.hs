module Main where

import Data.Attoparsec.Text
import Text.Subtitles.SRT
import qualified Data.Text.IO as I (readFile)
import System.IO (withFile, hSetNewlineMode, NewlineMode(..), Newline(CRLF), IOMode(WriteMode))

main :: IO ()
main = do
  content <- I.readFile "stuff/sub.srt"
  withFile "result.srt" WriteMode $ \handle -> do
    hSetNewlineMode handle (NewlineMode CRLF CRLF)
    case parseOnly parseSRT content of
      Left s -> putStrLn s
      Right _ -> putStrLn "Yay"
