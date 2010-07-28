{-# OPTIONS_GHC -W -Wall -fno-warn-unused-do-bind #-}
-- Copyright (c) 2009, Diego Souza
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

-- | Lexical analysis of yql statements
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

-- | Events the parser generates.
data ParserEvents c t v w s = ParserEvents { onTable    :: String -> t
                                           , onColumn   :: String -> c
                                           , onStrValue :: String -> v
                                           , onNumValue :: String -> v
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

parseYql :: String -> ParserEvents c t v w s -> Either ParseError s
parseYql input e = case tokStream
                   of Left err     -> Left err
                      Right input_ -> runParser parseYql_ () "stdin" input_
  where parseYql_ = parseSelect e
        
        tokStream = runParser scan "" "stdin" input

keyword :: String -> YqlParser String
keyword k0 = do k1 <- keyword_
                if (k1 == k0)
                  then return k0
                  else fail $ "expecting "++ (show $ TkKey k0)

symbol :: String -> YqlParser String
symbol s0 = do s1 <- symbol_
               if (s1 == s0)
                 then return s0
                 else fail $ "expecting "++ (show $ TkSym s0)

keyword_ :: YqlParser String
keyword_ = accept $ \t -> case t
                          of TkKey k -> Just k
                             _       -> Nothing

symbol_ :: YqlParser String
symbol_ = accept $ \t -> case t
                         of TkSym s -> Just s
                            _       -> Nothing

tkEof :: YqlParser ()
tkEof = accept $ \t -> case t
                       of TkEOF -> Just ()
                          _     -> Nothing

parseSelect :: ParserEvents c t v w s -> YqlParser s
parseSelect e = do keyword "SELECT"
                   cols <- parseColumns e
                   keyword "FROM"
                   table  <- parseTable e
                   ((keyword ";" >> tkEof)
                    <|> tkEof)
                   return (onSelect e cols table Nothing)

-- parseUpdate :: ParserEvents c t v w s -> YqlParser s
-- parseUpdate e = do keyword "UPDATE"
--                    table <- parseTable e
--                    keyword "SET"

parseColumns :: ParserEvents c t v w s -> YqlParser [c]
parseColumns e = fmap (const [onColumn e "*"]) (keyword "*")
                 <|> fmap (map (onColumn e)) (sepBy symbol_ (keyword ","))

parseTable :: ParserEvents c t v w s -> YqlParser t
parseTable e = fmap (onTable e) symbol_