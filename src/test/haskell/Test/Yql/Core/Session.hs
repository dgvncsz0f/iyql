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

module Test.Yql.Core.Session where

#define ok assertBool (__FILE__ ++":"++ show __LINE__)
#define eq assertEqual (__FILE__ ++":"++ show __LINE__)

import Yql.Core.Session
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import System.Directory
import System.Locale
import Data.Time
import Control.Monad (when)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

suite :: [Test]
suite = [ testGroup "Yql.Core.Session" [ test0
                                       , test1
                                       , test2
                                       , test3
                                       , test4
                                       , test5
                                       , test6
                                       , test7
                                       ]
        ]

token = fromApplication (Application "foo" "bar" (URL "foobaz"))

file = "/tmp/test-yql.core.session"

cleanup file = do exists <- doesFileExist file
                  when exists (removeFile file)
                  doesFileExist file >>= ok . not

test0 = testCase "test save with valid FileStorage works as expected" $
        do let storage = FileStorage file
           cleanup file
           save storage token
           cleanup file

test1 = testCase "test load with valid FileStorage returns token without changes " $
        do let storage = FileStorage file
           cleanup file
           save storage token
           saved <- load storage
           ok (Just token == saved)
           cleanup file

test2 = testCase "test load without any token returns Nothing" $
        do let storage = FileStorage file
           cleanup file
           saved <- load storage
           ok (Nothing == saved)

test3 = testCase "test unlink with valid FileStorage removes saved token" $
        do let storage = FileStorage file
           cleanup file
           save storage token
           doesFileExist file >>= ok
           unlink storage
           doesFileExist file >>= ok . not
           cleanup file

test4 = testCase "test mtime with valid FileStorage returns time token was saved" $
        do let storage = FileStorage file
           cleanup file
           now <- fmap (formatTime defaultTimeLocale "%s") getCurrentTime
           save storage token
           saved <- fmap (fmap (formatTime defaultTimeLocale "%s")) (mtime storage)
           ok (saved >= Just now)
           cleanup file

test5 = testCase "test mtime without with valid FileStorage but without any saved token returns Nothing" $
        do let storage = FileStorage file
           cleanup file
           saved <- mtime storage
           ok (Nothing == saved)

test6 = testCase "test mtime with DevNullStorage returns Nothing" $
        do save DevNullStorage token
           saved <- mtime DevNullStorage
           ok (Nothing == saved)

test7 = testCase "test load with DevNullStorage returns Nothing" $
        do save DevNullStorage token
           saved <- load DevNullStorage
           ok (Nothing == saved)
