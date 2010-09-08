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

-- | Test Trie module
module Test.Yql.Data.Trie where

#define ok assertBool (__FILE__ ++":"++ show __LINE__)
#define eq assertEqual (__FILE__ ++":"++ show __LINE__)

import Yql.Data.Trie as T
import Data.List (sort)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

suite :: [Test]
suite = [ testGroup "Trie.hs" [ test0
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

test0 = testCase "test empty function returns a empty trie" $
        do ok (T.null $ (empty :: Trie ()))

test1 = testCase "test fromList with empty list" $
        do ok (T.null $ (fromList [] :: Trie ()))

test2 = testCase "test fromList with null list = empty" $
        do eq (empty :: Trie ()) (fromList [])

test3 = testCase "test fromList with single value = singleton" $
        do eq (singleton [()]) (fromList [[()]])

test4 = testCase "toList . fromList = id" $
        do eq (list) (toList . fromList $ list)
  where list = sort [[0],[0,1],[0,1,2],[0,1,2,3]]

test5 = testCase "fromList . toList = id" $
        do eq (trie) (fromList . toList $ trie)
  where trie = fromList [[0],[0,1],[0,1,2],[0,1,2,3]] 

test6 = testCase "size empty = 0" $
        do eq (0) (size (empty :: Trie ()))

test7 = testCase "size singleton = 1" $
        do eq (1) (size $ singleton [()])

test8 = testCase "size fromList" $ 
        do eq (5) (size $ fromList [[0],[0,1],[0,1,2],[0,1,2,3],[4]])

test9 = testCase "subtrie performs prefix search" $
        do eq (fromList ["baz","bar"]) (subtrie "foo" $ fromList ["foobar","foobaz"])

test10 = testCase "subtrie with no valid prefix returns null" $
         do ok (T.null $ subtrie "zoo" $ fromList ["foobar","foobaz"])

test11 = testCase "union merges correctly two tries" $
         do eq (fromList ["foobar","foobaz"]) (fromList ["foobar"] `union` fromList ["foobaz"])

test12 = testCase "union null t = t" $
         do eq (fromList ["foobar"]) (empty `union` fromList ["foobar"])

test13 = testCase "union t null = t" $
         do eq (fromList ["foobar"]) (fromList ["foobar"] `union` empty)

test14 = testCase "test Ord instance" $
         do ok (fromList [[0]] < fromList [[1]])
            ok (fromList [[0]] <= fromList [[1]])
            ok (fromList [[0]] <= fromList [[0]])
            ok (fromList [[1]] > fromList [[0]])
            ok (fromList [[1]] >= fromList [[0]])
            ok (fromList [[0]] >= fromList [[0]])
            ok (empty >= (empty :: Trie ()))
            ok (empty <= (empty :: Trie ()))

test15 = testCase "test Eq instance" $
         do ok (fromList [[0]] == fromList [[0]])
            ok (fromList [[0]] /= fromList [[1]])
            ok (empty == (empty :: Trie ()))
