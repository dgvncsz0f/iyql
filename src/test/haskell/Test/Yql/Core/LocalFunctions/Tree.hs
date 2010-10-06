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

module Test.Yql.Core.LocalFunctions.Tree where

#define eq assertEqual (__FILE__ ++":"++ show __LINE__)
#define ok assertBool (__FILE__ ++":"++ show __LINE__)

import Yql.Data.PPrint
import Yql.Core.Types
import Yql.Core.LocalFunctions.Tree
import Yql.Core.LocalFunction
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

test0 = testCase ".tree() for `simple' results (i.e. rows without attributes)" $
        do eq tree (execTransform [] (function render) xml)
  where tree = init . unlines $ [ "Results"
                                , "+- row"
                                , "|  +- foo: foo"
                                , "|  +- bar: bar"
                                , "+- row"
                                , "|  +- foo: foo"
                                , "|  +- bar: bar"
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

test1 = testCase ".tree() for `complex' results (i.e. rows with attributes)" $
        do eq tabular (execTransform [] (function render) xml)
  where tabular = init . unlines $ [ "Results"
                                   , "+- row"
                                   , "|  +- foo"
                                   , "|  |  +- field_a: field_a"
                                   , "|  |  |  +- @attr: attr#0"
                                   , "|  |  +- field_b: field_b"
                                   , "|  +- bar: bar"
                                   , "+- row"
                                   , "|  +- foo: foo"
                                   , "|  +- bar"
                                   , "|  |  +- field_a: field_a"
                                   , "|  |  +- field_b: field_b"
                                   , "|  |  |  +- @attr: attr#1"
                                   , "|  |  +- field_c: field_c"
                                   ]

        xml = unlines [ "<?xml version=\"1.0\"?>"
                      , "<results>"
                      , "<row>"
                      , "<foo><field_a attr=\"attr#0\">field_a</field_a><field_b>field_b</field_b></foo>"
                      , "<bar>bar</bar>"
                      , "</row>"
                      , "<row>"
                      , "<foo>foo</foo>"
                      , "<bar><field_a>field_a</field_a><field_b attr=\"attr#1\">field_b</field_b><field_c>field_c</field_c></bar>"
                      , "</row>"
                      , "</results>"
                      ]

suite :: [Test]
suite = [ testGroup "Tables.hs" [ test0
                                , test1
                                ]
        ]
