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

module Test.Yql.UI.CLI.Commands.WhoAmI where

#define eq assertEqual (__FILE__ ++":"++ show __LINE__)
#define ok assertBool (__FILE__ ++":"++ show __LINE__)

import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Yql.Core.Session
import Yql.UI.CLI.Command
import Yql.UI.CLI.Commands.WhoAmI
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

newtype MySessionMgr = MySessionMgr (Maybe Token)

instance SessionMgr MySessionMgr where
  save _ _ = return ()
  load (MySessionMgr mtoken) = return mtoken
  unlink _ = return ()
  mtime _ = return Nothing

test0 = testCase "testing whoami without oauth token returns nobody" $ 
        do output <- bin (whoami (MySessionMgr Nothing)) "" []
           eq ("nobody") output

test1 = testCase "testing whoami with proper oauth token returns the guid" $
        do output <- bin (whoami (MySessionMgr (Just myToken))) "" []
           eq ("foobar") output
  where myToken = TwoLegg undefined (fromList [("xoauth_yahoo_guid","foobar")])

test2 = testCase "testing whoami without xoauth_yahoo_guid param returns nobody" $
        do output <- bin (whoami (MySessionMgr (Just myToken))) "" []
           eq ("nobody") output
  where myToken = TwoLegg undefined empty

suite :: [Test]
suite = [ testGroup "Commands/WhoAmI.hs" [ test0
                                        , test1
                                        , test2
                                        ]
        ]
