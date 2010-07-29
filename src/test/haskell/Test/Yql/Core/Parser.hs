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

-- | Test Parser module
module Test.Yql.Core.Parser where

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import Yql.Core.Lexer
import Yql.Core.Parser
import Text.ParserCombinators.Parsec
import Data.List

newtype LexerToken = LexerToken (String,TokenT)
                   deriving (Show)

stringBuilder = ParserEvents { onTable      = id
                             , onColumn     = id
                             , onTxtValue   = mkValue
                             , onNumValue   = id
                             , onMeValue    = "me"
                             , onSelect     = mkSelect
                             , onUpdate     = undefined
                             , onInsert     = undefined
                             , onDelete     = undefined
                             , onDesc       = undefined
                             , onEqExpr     = mkEqExpr
                             , onInExpr     = mkInExpr
                             , onAndExpr    = mkAndExpr
                             , onOrExpr     = mkOrExpr
                             }
  where mkValue v = "\"" ++ v ++ "\""
        
        mkSelect c t Nothing  = "SELECT " ++ (intercalate "," c) ++ " FROM " ++ t ++ ";"
        mkSelect c t (Just w) = "SELECT " ++ (intercalate "," c) ++ " FROM " ++ t ++ " WHERE " ++ w ++ ";"

        mkEqExpr c v = c ++"="++ v

        mkInExpr c vs = c ++ " IN ("++ intercalate "," vs ++")"

        mkAndExpr l r = l ++" AND "++ r

        mkOrExpr l r = l ++" OR "++ r

runYqlParser :: String -> String
runYqlParser input = case (parseYql input stringBuilder)
                     of Right output -> output
                        Left err     -> error ("failure: " ++ show err)

testParsingSelects = [ ("select * without where", H.assertEqual "" "SELECT * FROM iyql;" (runYqlParser "select * from iyql;"))
                     , ("select foo,bar tables without where", H.assertEqual "" "SELECT foo,bar FROM iyql;" (runYqlParser "select foo,bar from iyql;"))
                     , ("select * with single where clause (text)", H.assertEqual "" "SELECT * FROM iyql WHERE foo=\"bar\";" (runYqlParser "select * from iyql where foo=\"bar\";"))
                     , ("select * with single where clause (num)", H.assertEqual "" "SELECT * FROM iyql WHERE foo=3.141592653589793;" (runYqlParser "select * from iyql where foo=3.141592653589793;"))
                     , ("select * with multiple and/or expr", H.assertEqual "" "SELECT * FROM iyql WHERE foo=\"bar\" AND bar=\"foo\" OR id=me;" (runYqlParser "select * from iyql where foo=\"bar\" and bar=\"foo\" or id=me;"))
                     , ("select * with `in' where clause", H.assertEqual "" "SELECT * FROM iyql WHERE foo IN (\"b\",\"a\",\"r\",3,\".\",1);" (runYqlParser "select * from iyql where foo in (\"b\",\"a\",\"r\",3,\".\",1);"))
                     ]

suite :: [Test]
suite = [ testGroup "parsing select statements" (mk testParsingSelects)
        ]
  where mk = map (\t -> testCase (fst t) (snd t))