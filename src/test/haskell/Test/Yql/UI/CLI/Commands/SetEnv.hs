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

module Test.Yql.UI.CLI.Commands.SetEnv where

#define eq assertEqual (__FILE__ ++":"++ show __LINE__)
#define ok assertBool (__FILE__ ++":"++ show __LINE__)

import Yql.Core.Backend
import Yql.UI.CLI.Command
import qualified Yql.UI.CLI.Commands.SetEnv as E
import Data.List
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

newtype MyBackend = MyBackend [String]
                  deriving (Eq,Show)

instance Yql MyBackend where
  endpoint    = undefined
  credentials = undefined
  
  setenv (MyBackend es) e   = MyBackend (e:es)
  unsetenv (MyBackend es) e = MyBackend (delete e es)
  getenv (MyBackend es)     = es

test0 = testCase "setenv with leading + appends" $ 
        do output <- exec (E.setenv (MyBackend ["foo","bar"])) ["+foobar"]
           eq (Right $ MyBackend ["foobar","foo","bar"]) output

test1 = testCase "setenv with leading - deletes" $
        do output <- exec (E.setenv (MyBackend ["foobar","foo","bar"])) ["-foobar"]
           eq (Right $ MyBackend ["foo","bar"]) output

test2 = testCase "setenv without +/- define" $
        do output <- exec (E.setenv (MyBackend ["foo","bar"])) ["foobar"]
           eq (Right $ MyBackend ["foobar"]) output

test3 = testCase "setenv without args dump" $
        do output <- exec (E.setenv (MyBackend ["foo","bar"])) []
           eq (Left $ unlines ["foo","bar"]) output

suite :: [Test]
suite = [ testGroup "Commands/SetEnv.hs" [ test0
                                        , test1
                                        , test2
                                        , test3
                                        ]
        ]
