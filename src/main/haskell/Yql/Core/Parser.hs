{-# OPTIONS_GHC -W -Wall -fno-warn-unused-do-bind #-}
-- Copyright (c) 2010, Diego Souza
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--   * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright notice,
--     this list of conditions and the following disclaimer in the documentation
--     and/or other materials provided with the distribution.
--   * Neither the name of the <ORGANIZATION> nor the names of its contributors
--     may be used to endorse or promote products derived from this software
--     without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- | Syntactic analysis of Yql statements
module Yql.Core.Parser
       ( -- * Types
         ParserEvents(..)
         -- * Parser
       , parseYql
       )
       where

import Text.ParserCombinators.Parsec
import Yql.Core.Lexer

type YqlParser a = GenParser Token () a

-- | Events the parser generates. The main purpose of this is to allow
-- you constructing types that represents yql statements.
data ParserEvents c t v w s = ParserEvents { onTable    :: String -> t
                                           , onColumn   :: String -> c
                                           , onTxtValue :: String -> v
                                           , onNumValue :: String -> v
                                           , onMeValue  :: v
                                           , onSelect   :: [c] -> t -> Maybe w -> s
                                           , onUpdate   :: [(c,v)] -> t -> Maybe w -> s
                                           , onInsert   :: [(c,v)] -> t -> s
                                           , onDelete   :: t -> Maybe w -> s
                                           , onDesc     :: t -> s
                                           , onEqExpr   :: c -> v -> w
                                           , onInExpr   :: c -> [v] -> w
                                           , onAndExpr  :: w -> w -> w
                                           , onOrExpr   :: w -> w -> w
                                           }

-- | Parses an string, which must be a valid yql expression, using
-- ParserEvents to create generic types.
parseYql :: String -> ParserEvents c t v w s -> Either ParseError s
parseYql input e = case tokStream
                   of Left err     -> Left err
                      Right input_ -> runParser parseYql_ () "stdin" input_
  where parseYql_ = parseSelect e
        
        tokStream = runParser scan "" "stdin" input

quoted :: YqlParser String
quoted = accept test
  where test (TkStr s) = Just s
        test _         = Nothing

numeric :: YqlParser String
numeric = accept test
  where test (TkNum n) = Just n
        test _         = Nothing

keyword :: (String -> Bool) -> YqlParser String
keyword p = accept test
  where test (TkKey k) | p k       = Just k
                       | otherwise = Nothing
        test _                     = Nothing

symbol :: (String -> Bool) -> YqlParser String
symbol p = accept test
  where test (TkSym s) | p s       = Just s
                       | otherwise = Nothing
        test _                     = Nothing

symbol_ :: YqlParser String
symbol_ = symbol (const True)

anyTokenT :: YqlParser TokenT
anyTokenT = accept Just

tkEof :: YqlParser ()
tkEof = accept $ \t -> case t
                       of TkEOF -> Just ()
                          _     -> Nothing

parseSelect :: ParserEvents c t v w s -> YqlParser s
parseSelect e = do keyword (=="SELECT")
                   columns <- (fmap (const [onColumn e "*"]) (keyword (=="*"))
                               <|> parseColumn e `sepBy` keyword (==","))
                   keyword (=="FROM")
                   table  <- parseTable e
                   future <- lookAhead anyTokenT
                   case future
                     of TkKey "WHERE" -> do keyword (=="WHERE")
                                            w <- parseWhere e
                                            keyword (==";")
                                            tkEof
                                            return (onSelect e columns table (Just w))
                        _             -> do keyword (==";")
                                            tkEof
                                            return (onSelect e columns table Nothing)

parseColumn :: ParserEvents c t v w s -> YqlParser c
parseColumn e = fmap (onColumn e) symbol_

parseTable :: ParserEvents c t v w s -> YqlParser t
parseTable e = fmap (onTable e) symbol_

parseValue :: ParserEvents c t v w s -> YqlParser v
parseValue e = fmap (onTxtValue e) quoted 
               <|> fmap (onNumValue e) numeric
               <|> fmap (const $ onMeValue e) (keyword (=="ME"))

parseWhere :: ParserEvents c t v w s -> YqlParser w
parseWhere e = do column <- parseColumn e
                  op     <- keyword (`elem` ["=","IN"])
                  w      <- parseValueBy column op
                  future <- lookAhead anyTokenT
                  case future
                    of TkKey "AND" -> do anyTokenT
                                         fmap (onAndExpr e w) (parseWhere e)
                       TkKey "OR"  -> do anyTokenT
                                         fmap (onOrExpr e w) (parseWhere e)
                       _           -> return w
  where parseValueBy column "="  = fmap (onEqExpr e column) (parseValue e)
        parseValueBy column "IN" = do keyword (=="(")
                                      ret <- fmap (onInExpr e column) (parseValue e `sepBy` keyword (==","))
                                      keyword (==")")
                                      return ret
        parseValueBy _ _         = fail "expecting one of [=,IN]"
