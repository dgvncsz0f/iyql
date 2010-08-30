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

module Yql.UI.CLI.Commands.Parser
       ( parseCmd
       ) where

import Data.Char
import Text.ParserCombinators.Parsec

type CmdParser a = GenParser Char String a

parseCmd :: String -> Maybe (String,[String])
parseCmd input = case (runParser parser "" "stdin" input)
                 of Left _       -> Nothing
                    Right output -> Just output

parser :: CmdParser (String,[String])
parser = do many space
            command <- parseIdentifier
            (do many1 space
                args <- sepBy parseArgument (many1 space)
                return (command,args)) <|> return (command,[])
                     

parseIdentifier :: CmdParser String
parseIdentifier = do char ':'
                     many identifier
  where identifier = letter <|> digit <|> char '_'

parseArgument :: CmdParser String
parseArgument = do c <- lookAhead anyChar
                   case c of 
                     '\'' -> parseQuotedArg
                     '"'  -> parseQuotedArg
                     _    -> parseSimpleArg

parseQuotedArg :: CmdParser String
parseQuotedArg = do q <- oneOf ['"','\'']
                    value <- regular q
                    return value
  where regular q = do c <- anyChar
                       if (c==q)
                         then return []
                         else accept c q
        
        exceptional q = do c <- oneOf [q,'\\']
                           fmap (c:) (accept c q)
                       
        accept '\\' q          = exceptional q
        accept c q | c==q      = regular q
                   | otherwise = fmap (c:) (regular q)

parseSimpleArg :: CmdParser String
parseSimpleArg = many (satisfy (not . isSpace))
