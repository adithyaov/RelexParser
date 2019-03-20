{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Parsers
import Text.Parsec
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)

main :: IO ()
main = do
  args <- getArgs
  case args of
    x:y:xs -> saveToJson x y
    _ -> print "Insufficient args. Min 2 args required, from (.html) and to (.json)"

saveToJson rFrom sTo = do
  input <- readFile rFrom
  case parse mainParser rFrom input of
    Left err  -> print err
    Right w -> writeFile sTo $ unpack . encode $ w


-- Below are some examples for REPL testing

program1 = "<div><span>03:00</span><span>Yle uutiset</span></div>"
program2 = "<div><span>14:00</span><span><em>Kauhea kankkunen</em></span></div>"

channel = 
  "<div id=\"yle1\"> \n\
  \  <h3>Yle 1</h3> \n\
  \  <div><span>02:00</span><span>Yle uutiset</span></div> \n\
  \  <div><span>03:00</span><span>Yle uutiset</span></div> \n\
  \</div>"

day = 
  "<div class=\"day\" id=\"monday\"> \n\
  \  <h2>Monday</h2> \n\
  \  <div id=\"yle1\"> \n\
  \    <h3>Yle 1</h3> \n\
  \    <div><span>02:00</span><span>Yle uutiset</span></div> \n\
  \    <div><span>03:00</span><span>Yle uutiset</span></div> \n\
  \  </div> \n\
  \  </div>"


