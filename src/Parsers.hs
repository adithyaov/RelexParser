{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Parsers where

import Types
import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Text.Parsec
import Text.Parsec.String

-- Ignore all the attributes
html t = between (string ("<" ++ t) *> many (noneOf ">") *> char '>') (spaces *> string ("</" ++ t ++ ">"))

htmlText :: String -> Parser String
htmlText t = html t $ many $ noneOf "<"

-- Parsing Program
-- <div><span>02:00</span><span>Yle uutiset</span></div>
-- (or)
-- <div><span>02:00</span><span><em>Yle uutiset</em></span></div>
programParser = html "div" $ do
  t <- htmlText "span"
  n <- try (html "span" (htmlText "em")) <|> htmlText "span"
  return $ Program n t

-- Parsing Channel
-- <div id="yle1">
--   <h3>Yle 1</h3>
--   <div><span>02:00</span><span>Yle uutiset</span></div>
--   <div><span>03:00</span><span>Yle uutiset</span></div>
-- </div>
channelParser = html "div" $ do
  -- One can also extract the id if required
  c <- spaces *> htmlText "h3"
  ps <- many (try $ spaces *> programParser)
  return $ Channel c ps

-- Parsing Day
-- <div class="day" id="monday">
--   <h2>Monday</h2>
--   <div id="yle1">
--     ...
--   </div>
-- </div>
dayParser = html "div" $ do
  d <- spaces *> htmlText "h2"
  cs <- many (try $ spaces *> channelParser)
  return $ Day d cs

-- Parsing Week
-- <div id="content">
--   <div class="day" id="monday">
--     ...
--   </div>
-- </div>
weekParser = html "div" $ Week <$> many (try $ spaces *> dayParser)

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
mainParser = parseHtml <* (spaces *> eof)
  where
    parseHtml = html "html" $ do
      spaces *> html "head" spaces
      spaces *> html "body" (spaces *> weekParser)

