{-# OPTIONS_GHC -W -Wall -fno-warn-unused-do-bind #-}
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

module Yql.Core.Backend
       ( -- * Types
         Yql(..)
       , StdBackend(..)
       ) where

import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient
import Yql.Core.Stmt

-- | Minimum complete definition: endpoint, app.
class Yql y where
  -- | Returns the endpoint this backend is pointing to
  endpoint :: y -> String
  
  -- | Returns the application this backend is using to perform
  -- authenticated requests.
  app :: y -> Application
  
  -- | Given an statement executes the query on yql. This function is
  -- able to decide whenever that query requires authentication. If it
  -- does, it uses the oauth token in order to fullfil the request.
  execute :: (MonadIO m, HttpClient m) => y -> Statement -> m Response
  execute y stmt = runOAuth $ serviceRequest PLAINTEXT (Just "yahooapis.com") newRequest
    where newRequest = ReqHttp { version    = Http11
                               , ssl        = False
                               , host       = endpoint y
                               , port       = 80
                               , method     = GET
                               , reqHeaders = empty
                               , pathComps  = ["","v1","public","yql"]
                               , qString    = fromList [("q",show stmt)]
                               , reqPayload = B.empty
                               }

-- | Reference implementation.
data StdBackend m = StdBackend String Application (Token -> String -> OAuthMonad m ())

instance Yql (StdBackend m) where
  endpoint (StdBackend v _ _) = v
  
  app (StdBackend _ v _) = v