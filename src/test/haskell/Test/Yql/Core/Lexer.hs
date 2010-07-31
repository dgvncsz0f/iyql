{-# LANGUAGE CPP #-}
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

#define eq assertEqual (__FILE__ ++":"++ show __LINE__)

import Yql.Core.Lexer
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)
import Text.ParserCombinators.Parsec

newtype LexerToken = LexerToken (String,TokenT)
                   deriving (Show)

suite :: [Test]
suite = [ testGroup "Lexer.hs" [ test0
                               , test1
                               , test2
                               , test3
                               , test4
                               , test5
                               , test6
                               , test7
                               , test8
                               , test9
                               , test10
                               ]
        ]

test0 = testCase "keyword tokens" $ 
        do eq (TkKey "SELECT")  (scanToken1 "select")
           eq (TkKey "DELETE")  (scanToken1 "delete")
           eq (TkKey "INSERT")  (scanToken1 "insert")
           eq (TkKey "DELETE")  (scanToken1 "delete")
           eq (TkKey "DESC")    (scanToken1 "desc")
           eq (TkKey "WHERE")   (scanToken1 "where")
           eq (TkKey "SET")     (scanToken1 "set")
           eq (TkKey "VALUES")  (scanToken1 "values")
           eq (TkKey "OR")      (scanToken1 "or")
           eq (TkKey "AND")     (scanToken1 "and")
           eq (TkKey "IN")      (scanToken1 "in")
           eq (TkKey "INTO")    (scanToken1 "into")
           eq (TkKey "ME")      (scanToken1 "me")
           eq (TkKey "FROM")    (scanToken1 "from")
           eq (TkKey ";")       (scanToken1 ";")
           eq (TkKey ")")       (scanToken1 ")")
           eq (TkKey "(")       (scanToken1 "(")
           eq (TkKey "=")       (scanToken1 "=")
           eq (TkKey "*")       (scanToken1 "*")
           eq (TkKey ">")       (scanToken1 ">")
           eq (TkKey ">=")      (scanToken1 ">=")
           eq (TkKey "<")       (scanToken1 "<")
           eq (TkKey "<=")      (scanToken1 "<=")
           eq (TkKey "!=")      (scanToken1 "!=")
           eq (TkKey "|")       (scanToken1 "|")
           eq (TkKey "IS")      (scanToken1 "is")
           eq (TkKey "NOT")     (scanToken1 "not")
           eq (TkKey "NULL")    (scanToken1 "null")
           eq (TkKey "LIKE")    (scanToken1 "like")
           eq (TkKey "MATCHES") (scanToken1 "matches")

test1 = testCase "quoted string tokens" $ 
        do eq (TkStr "foobar")                (scanToken1 "'foobar'")
           eq (TkStr "select * from foobar;") (scanToken1 "'select * from foobar;'")
           eq (TkStr "")                      (scanToken1 "''")
           eq (TkStr "")                      (scanToken1 "\"\"")

test2 = testCase "quoted strings with escaped delimiters" $ 
        do eq (TkStr "foo'bar")  (scanToken1 "'foo\\'bar'")
           eq (TkStr "foo\"bar") (scanToken1 "\"foo\\\"bar\"")

test3 = testCase "integer numbers are numeric tokens" $ 
        eq (TkNum "7") (scanToken1 "7")

test4 = testCase "real numbers are numeric tokens" $
        do eq (TkNum "3.141592653589793") (scanToken1 "3.141592653589793")
           eq (TkNum "0.333333333333333") (scanToken1 "0.333333333333333")

test5 = testCase "foobar is a symbol" $ 
        eq (TkSym "foobar") (scanToken1 "foobar")

test6 = testCase "2..3 is a symbol" $ 
        eq (TkSym "2..3") (scanToken1 "2..3")

test7 = testCase "alphanum strings are symbols" $ 
        do eq (TkSym "foo.bar") (scanToken1 "foo.bar")
           eq (TkSym "foobaz") (scanToken1 "foobaz")

test8 = testCase "simple select statements" $ 
        do eq [ TkKey "SELECT",TkKey "*",TkKey "FROM",TkSym "foobar",TkKey "WHERE",TkSym "id",TkKey "=",TkKey "ME",TkKey ";",TkEOF] (scanToken "select * from foobar where id=me;")
           eq [TkKey "SELECT",TkSym "foo",TkKey ",",TkSym "bar",TkKey "FROM",TkSym "foobar",TkKey "WHERE",TkSym "id",TkKey "=",TkKey "ME",TkKey ";",TkEOF] (scanToken "select foo,bar from foobar where id=me;")

test9 = testCase "scan must ignore trailing and leading spaces" $
        do eq [TkSym "foobar",TkEOF] (scanToken "   foobar   ")
           eq [TkSym "foobar",TkEOF] (scanToken "\n\t  foobar   \n\n\t")

test10 = testCase "scan must ignore inner spaces" $
         do eq [TkSym "foo",TkSym "bar",TkEOF] (scanToken "foo    bar")
            eq [TkSym "foo",TkSym "bar",TkEOF] (scanToken "foo\t\n   bar")

runScan s f = case (runParser scan "" "" s)
              of Right x  -> f x
                 Left err -> error ("failure: "++ show err)

scanToken = flip runScan (map (snd.unToken))

scanToken1 = head . scanToken