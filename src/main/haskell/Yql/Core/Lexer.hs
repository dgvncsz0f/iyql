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

-- | Lexical analysis of yql statements
module Yql.Core.Lexer
       ( -- * Types
         TokenT(..)
       , Token(..)
         -- * Lexical Analysis
       , scan
       , accept
         -- * Other
       , keywords
       ) where

import Text.ParserCombinators.Parsec
import Data.Char

-- | Tokens emitted by the lexer.
data TokenT = TkKey String -- ^ Keywords (e.g.: SELECT, DESC, OR, *)
            | TkStr String -- ^ Quoted String (e.g.: 'foobar', "foobar")
            | TkNum String -- ^ Numeric (e.g.: 1, 1.2, .09999)
            | TkSym String -- ^ Any Symbol (e.g.: tables, colum names, etc.)
            | TkEOF
            deriving (Show,Eq)

-- | Relates a TokenT with the position in the stream, so that it can
-- feed the parser.
newtype Token = Token { unToken :: (SourcePos,TokenT) }
              deriving (Show,Eq)

-- | Performs the lexical analysis of a string and returns the tokens
-- found.
scan :: GenParser Char String [Token]
scan = do skipMany space
          t <- readToken
          skipMany space
          fmap (t:) (scan <|> (eof >> fmap (:[]) (mkToken TkEOF)))
  where readToken = quoted <|> symbol

-- | Parses a token created by the lexer so that you can use to
-- perform syntactic analysis.
accept :: (TokenT -> Maybe a) -> GenParser Token () a
accept p = token (show.snd.unToken) (fst.unToken) (p.snd.unToken)

mkToken :: TokenT -> GenParser Char String Token
mkToken t = do p <- getPosition
               return (Token (p,t))

symbol :: GenParser Char String Token
symbol = (fmap TkKey (try $ string ">=")
         <|> fmap TkKey (try $ string "<=")
         <|> fmap TkKey (try $ string "!=")
         <|> fmap (TkKey.(:[])) (oneOf single)
         <|> fmap mksym (many1 (satisfy sym))) >>= mkToken
  where single = ",;()=><|"

        sym c = (c `notElem` single) && not (isSpace c)

quoted :: GenParser Char String Token
quoted = do s       <- oneOf "'\""
            content <- loop s
            mkToken (TkStr content)
  where loop s = do c <- anyChar
                    case c
                      of '\\'          -> (char s >> fmap (s:) (loop s))
                                          <|> fmap (c:) (loop s)
                         x | s==x      -> return []
                           | otherwise -> fmap (x:) (loop s)

keywords :: [String]
keywords = [ "SELECT"
           , "UPDATE"
           , "INSERT"
           , "DELETE"
           , "DESC"
           , "USE"
           , "AS"
           , "SHOW"
           , "TABLES"
           , "OR"
           , "AND"
           , "IN"
           , "ME"
           , "FROM"
           , "WHERE"
           , "SET"
           , "VALUES"
           , "INTO"
           , "MATCHES"
           , "NULL"
           , "NOT"
           , "LIKE"
           , "IS"
           , "*"
           , "OFFSET"
           , "LIMIT"
           ]

mksym :: String -> TokenT
mksym sym | uSym `elem` keywords = TkKey uSym
          | tkNum sym            = TkNum sym
          | otherwise            = TkSym sym
            where uSym = map toUpper sym

                  tkNum (x:xs) | x=='.'    = not (null xs) && all isDigit xs
                               | otherwise = isDigit x && tkNum xs
                  tkNum []                 = True
