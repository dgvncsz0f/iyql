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

module Test.Yql.Core.LocalFunctions.Request where

#define eq assertEqual (__FILE__ ++":"++ show __LINE__)
#define ok assertBool (__FILE__ ++":"++ show __LINE__)

import Yql.Core.Types hiding (insert)
import Yql.Core.LocalFunctions.Request
import Yql.Core.LocalFunction
import Network.OAuth.Http.Request
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

test0 = testCase "endpoint is able to change hostname" $
        do eq (request { host="foobaz.com" }) (execBefore [("host",TxtValue "foobaz.com")] endpointFunction request)
  where Just request = parseURL "http://foobar.com/"


test1 = testCase "endpoint is able to change port" $
        do eq (request { port=8080 }) (execBefore [("port",NumValue "8080")] endpointFunction request)
  where Just request = parseURL "http://foobar.com/"

test2 = testCase "endpoint leaves request untouched if parameters are absent" $
        do eq request (execBefore [] endpointFunction request)
  where Just request = parseURL "http://foobar.com/"

test3 = testCase "json is able to modify the request" $
        do eq (request { qString = insert ("format","json") (qString request) }) (execBefore [] jsonFunction request)
  where Just request = parseURL "http://foobar.com/"

test4 = testCase "diagnostics is able to modify the request" $
        do eq (request { qString = insert ("diagnostics","true") (qString request)}) (execBefore [] diagnosticsFunction request)
  where Just request = parseURL "http://foobar.com/"

test5 = testCase "request modifies the request according parameters" $
        do eq (request { qString = foldr insert (qString request) [("foo","bar"),("bar","foo")]}) (execBefore [("foo",TxtValue "bar"),("bar",TxtValue "foo")] function request)
  where Just request = parseURL "http://foobar.com/"

suite :: [Test]
suite = [ testGroup "Request.hs" [ test0
                                 , test1
                                 , test2
                                 , test3
                                 , test4
                                 , test5
                                 ]
        ]
