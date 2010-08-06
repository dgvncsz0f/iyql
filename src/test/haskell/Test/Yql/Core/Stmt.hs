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

module Test.Yql.Core.Stmt where

#define eq assertEqual (__FILE__ ++":"++ show __LINE__)
#define ok assertBool (__FILE__ ++":"++ show __LINE__)

import Data.Maybe
import Yql.Core.Stmt
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

test0 = testCase "show select * without where produces correct stmt" $ 
        eq "SELECT * FROM iyql;" (show $ SELECT ["*"] "iyql" Nothing [])

test1 = testCase "show select foo,bar without where produces correct stmt" $
        eq "SELECT foo,bar FROM iyql;" (show $ SELECT ["foo","bar"] "iyql" Nothing [])

test2 = testCase "show select foo with single where clause [txt]" $
        eq "SELECT foo FROM iyql WHERE foo=\"bar\";" (show $ SELECT ["foo"] "iyql" (Just $ "foo" `OpEq` TxtValue "bar") [])
        
test3 = testCase "show select pi with single where clause [num]" $ 
        eq "SELECT pi FROM iyql WHERE pi=3.14;" (show $ SELECT ["pi"] "iyql" (Just $ "pi" `OpEq` NumValue "3.14") [])

test4 = testCase "show select foo with single IN clause" $
        eq "SELECT foo FROM iyql WHERE foo IN (\"b\",\"a\",\"r\");" (show $ SELECT ["foo"] "iyql" (Just $ "foo" `OpIn` [TxtValue "b",TxtValue "a",TxtValue "r"]) [])

test5 = testCase "show select with where expression with and/or" $
        do eq "SELECT foo FROM iyql WHERE foo=\"bar\" OR bar=\"foo\" AND pi=3.14;" (show $ SELECT ["foo"] "iyql" (Just $ ("foo" `OpEq` TxtValue "bar") `OpOr` ("bar" `OpEq` TxtValue "foo") `OpAnd` ("pi" `OpEq` NumValue "3.14")) [])

test6 = testCase "read string creates correct type" $
        do eq (SELECT ["foo","bar"] "iyql" (Just $ ("pi" `OpEq` NumValue "3.14") `OpOr` ("foo" `OpIn` [TxtValue "b",TxtValue "a",TxtValue "r"])) []) (read "select foo,bar from iyql where pi=3.14 or foo in (\"b\",\"a\",\"r\");")

test7 = testCase "show select escapes strings" $
        do eq "SELECT * FROM iyql WHERE foo=\"foo\\\"bar\";" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "foo\"bar") [])
           eq "SELECT * FROM iyql WHERE foo=\"foo'bar\";" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "foo'bar") [])

test8 = testCase "show select with remote functions" $ 
        do eq "SELECT * FROM iyql | iyql() | iyql(a=1) | iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" Nothing [Remote "iyql" [],Remote "iyql" [("a",NumValue "1")],Remote "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])
           eq "SELECT * FROM iyql WHERE foo=\"bar\" | iyql() | iyql(a=1) | iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "bar") [Remote "iyql" [],Remote "iyql" [("a",NumValue "1")],Remote "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])

test9 = testCase "show select with local functions" $ 
        do eq "SELECT * FROM iyql | .iyql() | .iyql(a=1) | .iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" Nothing [Local "iyql" [],Local "iyql" [("a",NumValue "1")],Local "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])
           eq "SELECT * FROM iyql WHERE foo=\"bar\" | .iyql() | .iyql(a=1) | .iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "bar") [Local "iyql" [],Local "iyql" [("a",NumValue "1")],Local "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])

test10 = testCase "pipeline [transform] is in correct order" $
         do eq (Just $ ("bar"++).("foo"++) $ ">") (fmap (($ ">") . transform) (pipeline myLinker [Local "foo" [],Local "bar" []]))
            eq (Just $ ("bar"++).("foo"++) $ ">") (fmap (($ ">") . transform) (pipeline myLinker [Local "foobar1" []]))
            eq (Just $ ("bar"++).(++"foo") $ ">") (fmap (($ ">") . transform) (pipeline myLinker [Local "foobar2" []]))
            eq (Just $ (++"bar").(++"foo") $ ">") (fmap (($ ">") . transform) (pipeline myLinker [Local "foobar3" []]))
            eq (Just $ (++"bar").("foo"++) $ ">") (fmap (($ ">") . transform) (pipeline myLinker [Local "foobar4" []]))
  where myLinker = [ ("foo", const (Just (Transform ("foo"++))))
                   , ("bar", const (Just (Transform ("bar"++))))
                   , ("foobar1", const (Just $ Transform ("foo"++) `Seq` Transform ("bar"++)))
                   , ("foobar2", const (Just $ Transform (++"foo") `Seq` Transform ("bar"++)))
                   , ("foobar3", const (Just $ Transform (++"foo") `Seq` Transform (++"bar")))
                   , ("foobar4", const (Just $ Transform ("foo"++) `Seq` Transform (++"bar")))
                   ] :: [(String,[(String,Value)] -> Maybe Exec)]

test11 = testCase "pipeline generates error when function is not found" $
         do ok (isNothing $ pipeline () [Local "foo" []])
            ok (isJust $ pipeline () [])

test12 = testCase "resolve [transform] is in correct order" $
         do eq (Just $ ("bar"++).("foo"++) $ ">") (fmap (($ ">") . transform) (resolve myLinker (SELECT ["*"] "foobar" Nothing [Local "foo" [],Local "bar" []])))
  where myLinker = [ ("foo", const (Just (Transform ("foo"++))))
                   , ("bar", const (Just (Transform ("bar"++))))
                   ] :: [(String,[(String,Value)] -> Maybe Exec)]

test13 = testCase "resolve generates error when function is not found" $
         do ok (isNothing $ resolve () (SELECT ["*"] "foobar" Nothing [Local "foo" []]))
            ok (isJust $ resolve () (SELECT ["*"] "foobar" Nothing []))

test14 = testCase "show desc produces correct result" $ 
         do eq ("DESC foobar;") (show $ DESC "foobar" [])
            eq ("DESC a;") (show $ DESC "a" [])
            eq ("DESC a | .yql();") (show $ DESC "a" [Local "yql" []])

test15 = testCase "read desc statements produces correct type" $ 
         do eq (DESC "foobar" []) (read "desc foobar;")
            eq (DESC "foobar" [Local "tables" []]) (read "desc foobar | .tables();")

suite :: [Test]
suite = [ testGroup "Stmt.hs" [ test0
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
                              , test11
                              , test12
                              , test13
                              , test14
                              , test15
                              ]
        ]
