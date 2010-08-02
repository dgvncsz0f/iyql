{-# OPTIONS_GHC -W -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances #-}
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
       , Backend(..)
       , Output()
       , OutputT()
         -- * Monads
       , unOutputT
       ) where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as B
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient
import Yql.Core.Stmt

newtype OutputT m a = OutputT { unOutputT :: m (Either String a) }

type Output a = Either String a

data Backend m = YqlBackend Application (Token -> String -> OAuthMonad m ())

-- | Minimum complete definition: endpoint, app.
class Yql y where
  -- | Returns the endpoint this backend is pointing to
  endpoint :: y -> String
  
  -- | Returns the application this backend is using to perform
  -- authenticated requests.
  app :: y -> Application
  
  -- | Given an statement executes the query on yql. This function is
  -- able to decide whenever that query requires authentication
  -- [TODO]. If it does, it uses the oauth token in order to fullfil
  -- the request.
  execute :: (Functor m,MonadIO m,HttpClient m,Linker l) => y -> l -> Statement -> OutputT m String
  execute y l stmt = do mkRequest  <- fmap before (resolve l stmt)
                        mkResponse <- fmap after (resolve l stmt)
                        mkOutput   <- fmap transform (resolve l stmt)
                        response   <- lift (runOAuth (serviceRequest PLAINTEXT (Just "yahooapis.com") (mkRequest yqlRequest)))
                        asString (mkResponse response) >>= return . mkOutput
                        
    where yqlRequest = ReqHttp { version    = Http11
                               , ssl        = False
                               , host       = endpoint y
                               , port       = 80
                               , method     = GET
                               , reqHeaders = empty
                               , pathComps  = ["","v1","public","yql"]
                               , qString    = fromList [("q",show preparedStmt)]
                               , reqPayload = B.empty
                               }
          
          preparedStmt = case stmt
                         of (SELECT c t w f) -> SELECT c t w (filter remote f)
                            _                -> stmt
          
          asString rsp | status rsp `elem` [200..299] = return (B.unpack . rspPayload $ rsp)
                       | otherwise                    = fail (B.unpack . rspPayload $ rsp)

instance Yql (Backend m) where
  endpoint (YqlBackend _ _) = "query.yahooapis.com"
  
  app (YqlBackend v _) = v

instance MonadTrans OutputT where
  lift = OutputT . liftM Right

instance MonadIO m => MonadIO (OutputT m) where
  liftIO mv = OutputT (liftIO $ do v <- mv
                                   return (Right v))

instance Monad m => Monad (OutputT m) where
  fail = OutputT . return . Left
  
  return = OutputT . return . Right
  
  (OutputT mv) >>= f = OutputT $ do v <- mv
                                    case v
                                      of Left msg -> return (Left msg)
                                         Right v' -> unOutputT (f v')

instance Monad m => Functor (OutputT m) where
  fmap f (OutputT mv) = OutputT $ do v <- mv
                                     case v
                                       of Left msg -> return (Left msg)
                                          Right v' -> return (Right $ f v')

instance Monad (Either String) where
  fail = Left
  
  return = Right
  
  m >>= f = case m
            of Right v  -> f v
               Left msg -> Left msg
