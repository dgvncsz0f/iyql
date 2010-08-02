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
data ParserEvents i v w f s = ParserEvents { onIdentifier :: String -> i
                                           , onTxtValue   :: String -> v
                                           , onNumValue   :: String -> v
                                           , onMeValue    :: v
                                           , onSelect     :: [i] -> i -> Maybe w -> [f] -> s
                                           , onUpdate     :: [(i,v)] -> i -> Maybe w -> s
                                           , onInsert     :: [(i,v)] -> i -> s
                                           , onDelete     :: i -> Maybe w -> s
                                           , onDesc       :: i -> s
                                           , onEqExpr     :: i -> v -> w
                                           , onInExpr     :: i -> [v] -> w
                                           , onAndExpr    :: w -> w -> w
                                           , onOrExpr     :: w -> w -> w
                                           , onLocalFunc  :: i -> [(i,v)] -> f
                                           , onRemoteFunc :: i -> [(i,v)] -> f
                                         }

-- | Parses an string, which must be a valid yql expression, using
-- ParserEvents to create generic types.
parseYql :: String -> ParserEvents i v w f s -> Either ParseError s
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

-- anyTokenT :: YqlParser TokenT
-- anyTokenT = accept Just

tkEof :: YqlParser ()
tkEof = accept $ \t -> case t
                       of TkEOF -> Just ()
                          _     -> Nothing

parseSelect :: ParserEvents i v w f s -> YqlParser s
parseSelect e = do keyword (=="SELECT")
                   c <- (fmap (const [onIdentifier e "*"]) (keyword (=="*"))
                         <|> parseIdentifier e `sepBy` keyword (==","))
                   keyword (=="FROM")
                   t <- parseIdentifier e
                   w <- whereClause 
                        <|> return Nothing
                   f <- (keyword (=="|") >> parseFunction e `sepBy` keyword (=="|"))
                        <|> return []
                   keyword (==";")
                   tkEof
                   return (onSelect e c t w f)
  where whereClause = do keyword (=="WHERE")
                         fmap Just (parseWhere e)

parseIdentifier :: ParserEvents i v w f s -> YqlParser i
parseIdentifier e = fmap (onIdentifier e) symbol_

parseValue :: ParserEvents i v w f s -> YqlParser v
parseValue e = fmap (onTxtValue e) quoted 
               <|> fmap (onNumValue e) numeric
               <|> fmap (const $ onMeValue e) (keyword (=="ME"))

parseWhere :: ParserEvents i v w f s -> YqlParser w
parseWhere e = do column <- parseIdentifier e
                  op     <- keyword (`elem` ["=","IN"])
                  w      <- parseValueBy column op
                  (keyword (=="AND") >> fmap (onAndExpr e w) (parseWhere e))
                   <|> (keyword (=="OR") >> fmap (onOrExpr e w) (parseWhere e))
                   <|> return w
  where parseValueBy column "="  = fmap (onEqExpr e column) (parseValue e)
        parseValueBy column "IN" = do keyword (=="(")
                                      ret <- fmap (onInExpr e column) (parseValue e `sepBy` keyword (==","))
                                      keyword (==")")
                                      return ret
        parseValueBy _ _         = fail "expecting one of [=,IN]"

parseFunction :: ParserEvents i v w f s -> YqlParser f
parseFunction e = do n <- symbol_
                     keyword (=="(")
                     argv <- arguments `sepBy` keyword (==",")
                     keyword (==")")
                     mkFunc n argv
  where arguments = do k <- parseIdentifier e
                       keyword (=="=")
                       v <- parseValue e
                       return (k,v)
        
        mkFunc ('.':n) argv = return (onLocalFunc e (onIdentifier e n) argv)
        mkFunc n argv       = return (onRemoteFunc e (onIdentifier e n) argv)
                     