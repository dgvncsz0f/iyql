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

module Main where

import Test.Framework
import qualified Test.Yql.Core.Lexer as A
import qualified Test.Yql.Core.Parser as B
import qualified Test.Yql.Core.Types as C
import qualified Test.Yql.Core.Backend as D
import qualified Test.Yql.Core.LocalFunctions.Request as E
import qualified Test.Yql.Data.Trie as F
import qualified Test.Yql.Core.Session as G
import qualified Test.Yql.Data.Cfg as H
import qualified Test.Yql.UI.CLI.Commands.Parser as I
import qualified Test.Yql.UI.CLI.Commands.WhoAmI as J
import qualified Test.Yql.UI.CLI.Commands.SetEnv as K
import qualified Test.Yql.Core.LocalFunctions.Tree as L

main :: IO ()
main = defaultMain $ concat [ A.suite
                            , B.suite
                            , C.suite
                            , D.suite
                            , E.suite
                            , F.suite
                            , G.suite
                            , H.suite
                            , I.suite
                            , J.suite
                            , K.suite
                            , L.suite
                            ]
