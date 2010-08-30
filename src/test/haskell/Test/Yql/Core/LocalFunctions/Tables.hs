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

module Test.Yql.Core.LocalFunctions.Tables where

#define eq assertEqual (__FILE__ ++":"++ show __LINE__)
#define ok assertBool (__FILE__ ++":"++ show __LINE__)

import Yql.Core.Types
import Yql.Core.LocalFunctions.Tables
import Yql.Core.LocalFunction
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

test0 = testCase ".tables() for `simple' results" $
        do eq tabular (execTransform (tablesTransform []) xml)
  where tabular = init . unlines $ [ "|*bar*|*foo*|"
                                   , "|bar  |foo  |"
                                   , "|bar  |foo  |"
                                   ]
        xml = unlines [ "<?xml version=\"1.0\"?>"
                      , "<results>"
                      , "<row>"
                      , "<foo>foo</foo>"
                      , "<bar>bar</bar>"
                      , "</row>"
                      , "<row>"
                      , "<foo>foo</foo>"
                      , "<bar>bar</bar>"
                      , "</row>"
                      , "</results>"
                      ]

test1 = testCase ".tables() for `complex' results" $
        do eq tabular (execTransform (tablesTransform []) xml)
  where tabular = init . unlines $ [ "|*bar*                          |*foo*                |"
                                   , "|bar                            ||*field_a*|*field_b*|"
                                   , "                                 |field_a  |field_b  ||"
                                   , "||*field_a*|*field_b*|*field_c*|"
                                   , " |field_a  |field_b  |field_c  ||foo                  |"
                                   ]

        xml = unlines [ "<?xml version=\"1.0\"?>"
                      , "<results>"
                      , "<row>"
                      , "<foo><field_a>field_a</field_a><field_b>field_b</field_b></foo>"
                      , "<bar>bar</bar>"
                      , "</row>"
                      , "<row>"
                      , "<foo>foo</foo>"
                      , "<bar><field_a>field_a</field_a><field_b>field_b</field_b><field_c>field_c</field_c></bar>"
                      , "</row>"
                      , "</results>"
                      ]

test2 = testCase ".tables() add missing rows" $
        do eq tabular (execTransform (tablesTransform []) xml)
  where tabular = init . unlines $ [ "|*bar*|*foo*|"
                                   , "|0    |0    |"
                                   , "|1    |     |"
                                   , "|2    |     |"
                                   ]

        xml = unlines [ "<?xml version=\"1.0\"?>"
                      , "<results>"
                      , "<row>"
                      , "<foo>0</foo>"
                      , "<bar>0</bar>"
                      , "</row>"
                      , "<row>"
                      , "<bar>1</bar>"
                      , "</row>"
                      , "<row>"
                      , "<bar>2</bar>"
                      , "</row>"
                      , "</results>"
                      ]

test3 = testCase ".tables() fill columns with spaces" $
        do eq tabular (execTransform (tablesTransform []) xml)
  where tabular = init . unlines $ [ "|*bar*      |*foo*     |"
                                   , "|0          |1         |"
                                   , "|fooooooobar|foooooobar|"
                                   ]
        xml = unlines [ "<?xml version=\"1.0\"?>"
                      , "<results>"
                      , "<row>"
                      , "<foo>1</foo>"
                      , "<bar>0</bar>"
                      , "</row>"
                      , "<row>"
                      , "<bar>fooooooobar</bar>"
                      , "<foo>foooooobar</foo>"
                      , "</row>"
                      , "</results>"
                      ]

suite :: [Test]
suite = [ testGroup "Tables.hs" [ test0
                                , test1
                                , test2
                                , test3
                                ]
        ]
