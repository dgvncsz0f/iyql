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

import System.FilePath
import Yql.Core.Backend
import Yql.Core.Session
import Yql.UI.Cli
import Yql.Data.Cfg
import Network.OAuth.Consumer

main :: IO ()
main = do session <- fmap mkSession basedir
          config  <- usrCfg
          iyql session (backend session config)
  where backend session config = YqlBackend (Application cfgCKey cfgCSec OOB) session (tryCfgs config "env" []) yqlEndpoint
          where cfgCKey     = tryCfg config "oauth_consumer_key" "iyql"
                cfgCSec     = tryCfg config "oauth_consumer_sec" ""
                yqlEndpoint = let (host,port) = break (==':') (tryCfg config "endpoint" "query.yahooapis.com:80")
                              in case port
                                 of []    -> (host,80)
                                    [':'] -> (host,80)
                                    (_:p) -> (host,read p)
        
        mkSession home = FileStorage (joinPath [home,"oauth_token"])
