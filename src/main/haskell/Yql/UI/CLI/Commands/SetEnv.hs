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

module Yql.UI.CLI.Commands.SetEnv
       ( setenv
       ) where

import qualified Yql.Core.Backend as Y
import Yql.UI.CLI.Command

-- | Removes any saved oauth_token.
setenv :: Y.Yql y => y -> Command (Either String y)
setenv be0 = Command (doc, const exe)
  where doc n  = unlines [ "Modifies the env parameters that are sent to yql"
                         , "Examples:"
                         , "    :"++n++" +foobar  :: Appends the `foobar' string to the env list"
                         , "    :"++n++" -foobar  :: Removes the `foobar' string from the env list"
                         , "    :"++n++" foobar   :: Resets the env, leaving only `foobar' string defined"
                         , "    :"++n++"          :: Dump the current env list"
                         ]
        exe [] = return (Left $ unlines (Y.getenv be0))
        exe es = return (Right $ foldr fold be0 es)
          where fold ('+':env) be = Y.setenv be env
                fold ('-':env) be = Y.unsetenv be env
                fold (env) be     = Y.setenv (foldr (flip Y.unsetenv) be (Y.getenv be)) env
