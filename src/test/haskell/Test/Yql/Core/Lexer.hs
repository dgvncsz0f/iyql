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

-- | Test Lexer module
module Test.Yql.Core.Lexer where

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import Yql.Core.Lexer
import Text.ParserCombinators.Parsec

newtype LexerToken = LexerToken (String,TokenT)
                   deriving (Show)

runScan s f = case (runParser scan "" "" s)
              of Right x  -> f x
                 Left err -> error ("failure: "++ show err)

runScanToken = flip runScan (map (snd.unToken))

testScanKeywords = [ ("select keyword", H.assertEqual "" [TkKey "SELECT",TkEOF] (runScanToken "select"))
                   , ("update keyword", H.assertEqual "" [TkKey "UPDATE",TkEOF] (runScanToken "update"))
                   , ("delete keyword", H.assertEqual "" [TkKey "DELETE",TkEOF] (runScanToken "delete"))
                   , ("insert keyword", H.assertEqual "" [TkKey "INSERT",TkEOF] (runScanToken "insert"))
                   , ("desc keyword", H.assertEqual "" [TkKey "DESC",TkEOF] (runScanToken "desc"))
                   , ("where keyword", H.assertEqual "" [TkKey "WHERE",TkEOF] (runScanToken "where"))
                   , ("set keyword", H.assertEqual "" [TkKey "SET",TkEOF] (runScanToken "set"))
                   , ("values keyword", H.assertEqual "" [TkKey "VALUES",TkEOF] (runScanToken "values"))
                   , ("or keyword", H.assertEqual "" [TkKey "OR",TkEOF] (runScanToken "or"))
                   , ("and keyword", H.assertEqual "" [TkKey "AND",TkEOF] (runScanToken "and"))
                   , ("in keyword", H.assertEqual "" [TkKey "IN",TkEOF] (runScanToken "in"))
                   , ("into keyword", H.assertEqual "" [TkKey "INTO",TkEOF] (runScanToken "into"))
                   , ("me keyword", H.assertEqual "" [TkKey "ME",TkEOF] (runScanToken "me"))
                   , ("from keyword", H.assertEqual "" [TkKey "FROM",TkEOF] (runScanToken "from"))
                   , ("; keyword", H.assertEqual "" [TkKey ";",TkEOF] (runScanToken ";"))
                   , (") keyword", H.assertEqual "" [TkKey ")",TkEOF] (runScanToken ")"))
                   , ("( keyword", H.assertEqual "" [TkKey "(",TkEOF] (runScanToken "("))
                   , ("= keyword", H.assertEqual "" [TkKey "=",TkEOF] (runScanToken "="))
                   , ("* keyword", H.assertEqual "" [TkKey "*",TkEOF] (runScanToken "*"))
                   ]

testScanQuotedStrings = [ ("single-quote string", H.assertEqual "" [TkStr "foobar",TkEOF] (runScanToken "'foobar'"))
                        , ("double-quote string", H.assertEqual "" [TkStr "foobar",TkEOF] (runScanToken "\"foobar\""))
                        , ("quoted string with keywords", H.assertEqual "" [TkStr "select * from foobar;",TkEOF] (runScanToken "'select * from foobar;'"))
                        , ("empty single-quote string", H.assertEqual "" [TkStr "",TkEOF] (runScanToken "''"))
                        , ("empty double-quote string", H.assertEqual "" [TkStr "",TkEOF] (runScanToken "\"\""))
                        -- , ("double-quote string with escaped double-quotes", H.assertEqual "" [TkStr "\"foobar\"",TkEOF] (runScanToken "\"\\\"foobar\\\"\""))
                        -- , ("single-quote string with escaped single-quotes", H.assertEqual "" [TkStr "'foobar'",TkEOF] (runScanToken "'\'foobar\''"))
                        ]

testNumeric = [ ("integer is numeric", H.assertEqual "" [TkNum "7",TkEOF] (runScanToken "7"))
              , ("real is numeric", H.assertEqual "" [TkNum "3.141592653589793",TkEOF] (runScanToken "3.141592653589793"))
              , ("anything else is sym", H.assertEqual "" [TkSym "0..9",TkEOF] (runScanToken "0..9"))
              , ("without integral part is num", H.assertEqual "" [TkNum ".7",TkEOF] (runScanToken ".7"))
              , ("dot only is sym", H.assertEqual "" [TkSym ".",TkEOF] (runScanToken "."))
              ]

testSymbols = [ ("foobar is a symbol", H.assertEqual "" [TkSym "foobar",TkEOF] (runScanToken "foobar"))
              ]

testStatements = [ ("select", H.assertEqual "" [TkKey "SELECT",TkKey "*",TkKey "FROM",TkSym "foobar",TkKey "WHERE",TkSym "id",TkKey "=",TkKey "ME",TkKey ";",TkEOF] (runScanToken "select * from foobar where id=me;"))
                 , ("select", H.assertEqual "" [TkKey "SELECT",TkSym "foo",TkKey ",",TkSym "bar",TkKey "FROM",TkSym "foobar",TkKey "WHERE",TkSym "id",TkKey "=",TkKey "ME",TkKey ";",TkEOF] (runScanToken "select foo,bar from foobar where id=me;"))
                 , ("update", H.assertEqual "" [TkKey "UPDATE",TkSym "foobar",TkKey "SET",TkSym "name",TkKey "=",TkStr "foobaz",TkKey "WHERE",TkSym "id",TkKey "=",TkKey "ME",TkKey ";",TkEOF] (runScanToken "update foobar set name=\"foobaz\" where id=me;"))
                 , ("delete", H.assertEqual "" [TkKey "DELETE",TkKey "FROM",TkSym "foobar",TkKey "WHERE",TkSym "id",TkKey "=",TkKey "ME",TkKey ";",TkEOF] (runScanToken "delete from foobar where id=me;"))
                 , ("insert", H.assertEqual "" [TkKey "INSERT",TkKey "INTO",TkSym "foobar",TkKey "(",TkSym "name",TkKey ",",TkSym "id",TkKey ")",TkKey "VALUES",TkKey "(",TkStr "foobar",TkKey ",",TkKey "ME",TkKey ")",TkKey ";",TkEOF] (runScanToken "insert into foobar (name,id) values (\"foobar\",me);"))
                 , ("desc", H.assertEqual "" [TkKey "DESC",TkSym "foobar",TkKey ";",TkEOF] (runScanToken "desc foobar;"))
                 ]

testSpaces = [ ("ingores newlines", H.assertEqual "" [TkKey "SELECT",TkKey "*",TkKey "FROM",TkSym "foobar",TkKey ";",TkEOF] (runScanToken "\nselect\n*\nfrom\nfoobar;\n"))
             , ("ignores spaces", H.assertEqual "" [TkKey "SELECT",TkKey "*",TkKey "FROM",TkSym "foobar",TkKey ";",TkEOF] (runScanToken "   select   *   from   foobar;  "))
             , ("ignores tabs", H.assertEqual "" [TkKey "SELECT",TkKey "*",TkKey "FROM",TkSym "foobar",TkKey ";",TkEOF] (runScanToken "\tselect\t\t*\t\tfrom\t\tfoobar;\t"))
             , ("ignores nbsp", H.assertEqual "" [TkKey "SELECT",TkKey "*",TkKey "FROM",TkSym "foobar",TkKey ";",TkEOF] (runScanToken " select    *    from    foobar;   "))
             ]

suite :: [Test]
suite = [ testGroup "scan keywords" (mk testScanKeywords)
        , testGroup "quoted strings" (mk testScanQuotedStrings)
        , testGroup "numeric" (mk testNumeric)
        , testGroup "symbols" (mk testSymbols)
        , testGroup "statements" (mk testStatements)
        , testGroup "spaces" (mk testSpaces)
        ]
  where mk = map (\t -> testCase (fst t) (snd t))