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

module Test.Yql.UI.CLI.Commands.Parser where

#define eq assertEqual (__FILE__ ++":"++ show __LINE__)
#define ok assertBool (__FILE__ ++":"++ show __LINE__)

import Yql.UI.CLI.Commands.Parser
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

test0 = testCase "testing parseCmd without arguments" $ 
        do eq (Just $ ("foobar",[])) (parseCmd ":foobar")

test1 = testCase "testing parseCmd with trailing and leading spaces" $
        do eq (Just $ ("foobar",[])) (parseCmd "  :foobar")
           eq (Just $ ("foobar",[])) (parseCmd ":foobar   ")
           eq (Just $ ("foobar",[])) (parseCmd "  :foobar  ")
           eq (Just $ ("foobar",["foo","bar"])) (parseCmd "  :foobar  foo   bar")

test2 = testCase "testing parseCmd with arguments [simple]" $
        do eq (Just $ ("foobar",["f","o","o","b","a","r"])) (parseCmd ":foobar f o o b a r")

test3 = testCase "testing parseCmd with arguments [quoted]" $
        do eq (Just $ ("foobar",["f o o b a r","r a b o o f"])) (parseCmd ":foobar 'f o o b a r' \"r a b o o f\"")

test4 = testCase "testing parseCmd with arguments [quoted with \\' and \\\"]" $
        do eq (Just $ ("foobar",["f'oobar","f\"oobar"])) (parseCmd ":foobar 'f\\'oobar' \"f\\\"oobar\"")
           eq (Just $ ("foobar",["f\\'oobar"])) (parseCmd ":foobar 'f\\\\'oobar'")

test5 = testCase "testing parseCmd with arguments with trailing spaces" $
        do eq (Just $ ("foobar",["foobar"])) (parseCmd ":foobar foobar   ")

test6 = testCase "testing parseCmd with multiple spaces separating arguments" $
        do eq (Just $ ("foobar",["foobar","foobaz"])) (parseCmd ":foobar           foobar     foobaz")

suite :: [Test]
suite = [ testGroup "Commands/Parser.hs" [ test0
                                        , test1
                                        , test2
                                        , test3
                                        , test4
                                        , test5
                                        , test6
                                        ]
        ]
