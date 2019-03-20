{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parsers
import Text.Parsec

main :: IO ()
main = putStrLn "Hello, Haskell!"

parseFromFile fname = do
  input <- readFile fname
  print $ runParser mainParser () fname input

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


