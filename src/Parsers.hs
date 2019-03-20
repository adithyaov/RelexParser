{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import Control.Applicative hiding ((<?>))
import Control.Monad
import Data.Attoparsec.Text
import Data.Functor
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment
import Types

html t p = string ("<" <> t <> ">") *> p <* string ("</" <> t <> ">")

htmlText t = html t $ takeTill (/= '<')

spaces = many' space

-- Parsing Program
-- <div><span>02:00</span><span>Yle uutiset</span></div>
-- (or)
-- <div><span>02:00</span><span><em>Yle uutiset</em></span></div>
programParser = do
  string "<div>"
  t <- htmlText "span"
  n <- html "span" (htmlText "em") <|> htmlText "span"
  string "</div>"
  return $ Program n t

-- Parsing Channel
-- <div id="yle1">
--   <h3>Yle 1</h3>
--   <div><span>02:00</span><span>Yle uutiset</span></div>
--   <div><span>03:00</span><span>Yle uutiset</span></div>
-- </div>
channelParser = do
  -- One can also extract the id
  string "<div id=\"" *> takeTill (/= '"') <* string "\">"
  c <- spaces *> htmlText "h3"
  ps <- many' (spaces *> programParser)
  spaces *> string "</div>"
  return $ Channel c ps

-- Parsing Day
-- <div class="day" id="monday">
--   <h2>Monday</h2>
--   <div id="yle1">
--     ...
--   </div>
-- </div>
dayParser = do
  string "<div class=\"day\" id=\"" *> takeTill (/= '"') <* string "\">"
  d <- spaces *> htmlText "h2"
  cs <- many' (spaces *> channelParser)
  spaces *> string "</div>"
  return $ Day d cs

-- Parsing Week
-- <div id="content">
--   <div class="day" id="monday">
--     ...
--   </div>
-- </div>
weekParser = do
  string "<div id=\"content\">"
  ds <- many' (spaces *> dayParser)
  return $ Week ds

-- Parsing Everything else
-- <html>
--   <head>
--   </head>
--   <body>
--     <div id="content">
--       ...
--     </div>
--   </body>
-- </html>
mainParser =
  html "html" $ do
    spaces *> html "head" spaces
    spaces *> html "body" (spaces *> weekParser)
