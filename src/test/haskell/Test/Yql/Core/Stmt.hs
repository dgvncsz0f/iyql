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

import Yql.Core.Stmt
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

test0 = testCase "show select * without where produces correct stmt" $ 
        eq "SELECT * FROM iyql;" (show $ SELECT [Column "*"] (Table "iyql") Nothing)

test1 = testCase "show select foo,bar without where produces correct stmt" $
        eq "SELECT foo,bar FROM iyql;" (show $ SELECT [Column "foo",Column "bar"] (Table "iyql") Nothing)

test2 = testCase "show select foo with single where clause [txt]" $
        eq "SELECT foo FROM iyql WHERE foo=\"bar\";" (show $ SELECT [Column "foo"] (Table "iyql") (Just $ Column "foo" `OpEq` TxtValue "bar"))
        
test3 = testCase "show select pi with single where clause [num]" $ 
        eq "SELECT pi FROM iyql WHERE pi=3.14;" (show $ SELECT [Column "pi"] (Table "iyql") (Just $ Column "pi" `OpEq` NumValue "3.14"))

test4 = testCase "show select foo with single IN clause" $
        eq "SELECT foo FROM iyql WHERE foo IN (\"b\",\"a\",\"r\");" (show $ SELECT [Column "foo"] (Table "iyql") (Just $ Column "foo" `OpIn` [TxtValue "b",TxtValue "a",TxtValue "r"]))

test5 = testCase "show select with where expression with and/or" $
        do eq "SELECT foo FROM iyql WHERE foo=\"bar\" OR bar=\"foo\" AND pi=3.14;" (show $ SELECT [Column "foo"] (Table "iyql") (Just $ (Column "foo" `OpEq` TxtValue "bar") `OpOr` (Column "bar" `OpEq` TxtValue "foo") `OpAnd` (Column "pi" `OpEq` NumValue "3.14")))

test6 = testCase "read string creates correct type" $
        do eq (SELECT [Column "foo",Column "bar"] (Table "iyql") (Just $ (Column "pi" `OpEq` NumValue "3.14") `OpOr` (Column "foo" `OpIn` [TxtValue "b",TxtValue "a",TxtValue "r"]))) (read "select foo,bar from iyql where pi=3.14 or foo in (\"b\",\"a\",\"r\");")

test7 = testCase "show select escapes strings" $
        do eq "SELECT * FROM iyql WHERE foo=\"foo\\\"bar\";" (show $ SELECT [Column "*"] (Table "iyql") (Just $ Column "foo" `OpEq` TxtValue "foo\"bar"))
           eq "SELECT * FROM iyql WHERE foo=\"foo'bar\";" (show $ SELECT [Column "*"] (Table "iyql") (Just $ Column "foo" `OpEq` TxtValue "foo'bar"))

suite :: [Test]
suite = [ testGroup "Stmt.hs" [ test0
                              , test1
                              , test2
                              , test3
                              , test4
                              , test5
                              , test6
                              , test7
                              ]
        ]