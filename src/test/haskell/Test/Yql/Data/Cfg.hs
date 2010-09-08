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

module Test.Yql.Data.Cfg where

#define eq assertEqual (__FILE__ ++":"++ show __LINE__)
#define ok assertBool (__FILE__ ++":"++ show __LINE__)
#define mycfg __FILE__ ++"/cfg"

import Yql.Data.Cfg
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

test0 = testCase "testing trycfg returns default when property is not defined" $ 
        do eq "foobar" (tryCfg empty "foobar" "foobar")

test1 = testCase "testing trycfg returns the value defined in the config file" $
        do eq "oobar" (tryCfg (fromList [("f","oobar")]) "f" "")

test2 = testCase "testing parseCfg ignores comments" $
        do eq (fromList [("foo","bar")]) (parseCfg "--foobar\nfoo: bar\n")
           eq (fromList [("foo","bar ")]) (parseCfg "--foobar\nfoo: bar --foobar\n")

test3 = testCase "testing parseCfg ignores spaces" $
        do eq (fromList [("foo","bar")]) (parseCfg "\n\n   \nfoo: bar\n\n\n")

test4 = testCase "parseCfg ignores comments with -- " $
        do eq (fromList [("foo","bar")]) (parseCfg "-- foo -- bar --\n\nfoo:bar-- another -- comment")

test5 = testCase "tryCfgs returns the default when no key is found" $
        do eq (["foobar"]) (tryCfgs (fromList []) "foobar" ["foobar"])

test6 = testCase "tryCfgs returns the keys found when there is at least one match" $
        do eq (["foobar"]) (tryCfgs (fromList [("f","foobar"),("o","oobar")]) "f" ["error"])

test7 = testCase "tryCfgs returns all keys found" $
        do eq (["foobar","foobaz","fuba"]) (tryCfgs (fromList [("f","foobar"),("f","foobaz"),("f","fuba")]) "f" ["error"])

suite :: [Test]
suite = [ testGroup "Cfg.hs" [ test0
                             , test1
                             , test2
                             , test3
                             , test4
                             , test5
                             , test6
                             , test7
                             ]
        ]
