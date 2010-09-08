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
       , OutputT()
         -- * Monads
       , unOutputT
       ) where

import Yql.Data.Cfg
import Yql.Core.Types
import qualified Yql.Core.LocalFunctions.Request as F
import Yql.Core.LocalFunction
import Yql.Core.Session
import Yql.Data.Xml
import Data.Char
import Data.Maybe
import qualified Data.List as L
import Data.Time
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Codec.Binary.UTF8.String as U
import Network.OAuth.Consumer hiding (application)
import qualified Network.OAuth.Http.Request as R
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient

newtype OutputT m a = OutputT { unOutputT :: m (Either String a) }

-- | The reference implementation backend available
data Backend s = YqlBackend { application  :: Application  -- ^ The applicat that is used to perform 3-legged oauth requests
                            , sessionMgr   :: s            -- ^ Used for saving/loading oauth tokens
                            , defaultEnv   :: [String]     -- ^ Default environment to use when performing requests
                            , myEndpoint   :: (String,Int) -- ^ The hostname and port of yql server
                            }

-- | Tells the minimum security level required to perform this
-- statament.
descTablesIn :: (MonadIO m,HttpClient m,Yql y) => y -> [String] -> Expression -> OutputT m Description
descTablesIn y envs stmt = case stmt
                           of _ | desc stmt       -> return $ foldr1 joinDesc (map (const $ Table "<<many>>" Any False) (tables stmt))
                                | usingMe stmt    -> return $ foldr1 joinDesc (map (const $ Table "<<many>>" User False) (tables stmt))
                                | showTables stmt -> return (Table "<<dummy>>" Any False)
                                | otherwise       -> fmap (foldr1 joinDesc) (mapM execDesc (tables stmt))
    where parseXml xml = case (xmlParse xml)
                         of Just doc -> case (readDescXml doc)
                                        of Just rst -> return rst
                                           Nothing  -> fail $ "couldn't desc tables " ++ L.intercalate "," (tables stmt)
                            Nothing  -> fail "error parsing xml"
          
          execDesc t = execute y database (mkDesc stmt t) >>= parseXml
            where database = M.fromList [("request", F.function)]
          
          mkDesc (USE u a stmt') t = USE u a (mkDesc stmt' t)
          mkDesc _ t               = DESC t funcs
          
          joinDesc (Table _ sec0 ssl0) (Table _ sec1 ssl1) = Table "<<many>>" (max sec0 sec1) (ssl0 || ssl1)
          
          funcs | null envs = [] 
                | otherwise = [Local "request" [("env",TxtValue env) | env <- envs]]

emptyRequest :: R.Request
emptyRequest = R.ReqHttp { R.version    = R.Http11
                         , R.ssl        = False
                         , R.host       = "query.yahooapis.com"
                         , R.port       = 80
                         , R.method     = R.GET
                         , R.reqHeaders = R.empty
                         , R.pathComps  = ["","v1","public","yql"]
                         , R.qString    = R.empty
                         , R.reqPayload = B.empty
                         }

mkRequest :: Expression -> [String] -> Description -> R.Request
mkRequest stmt env d = emptyRequest { R.ssl        = https d
                                    , R.port       = portNumber
                                    , R.pathComps  = yqlPath
                                    , R.method     = httpMethod
                                    , R.qString    = R.fromList $ ("q",show $ preparedStmt stmt) : map (\e -> ("env",e)) env
                                    }
  where yqlPath
          | security d `elem` [User,App] = ["","v1","yql"]
          | otherwise                    = ["","v1","public","yql"]

        portNumber
          | https d   = 443
          | otherwise = 80

        httpMethod | update stmt = R.PUT
                   | insert stmt = R.PUT
                   | delete stmt = R.DELETE
                   | otherwise   = R.GET

        preparedStmt stmt_ = case stmt_
                             of (SELECT c t w rl ll f) -> SELECT c t w rl ll (filter remote f)
                                (DESC t _)             -> DESC t []
                                (UPDATE c t w f)       -> UPDATE c t w (filter remote f)
                                (INSERT c t f)         -> INSERT c t (filter remote f)
                                (DELETE t w f)         -> DELETE t w (filter remote f)
                                (SHOWTABLES f)         -> SHOWTABLES (filter remote f)
                                (USE url as stmt')     -> USE url as (preparedStmt stmt')

-- | Minimum complete definition: endpoint, app.
class Yql y where
  -- | Returns the endpoint this backend is pointing to
  endpoint :: y -> (String,Int)
  endpoint _ = ("query.yahooapis.com",80)
  
  -- | Add a new env that is included in the request
  setenv :: y -> String -> y
  
  -- | Delete an entry from the environment
  unsetenv :: y -> String -> y
  
  -- | Returns the env that are currently loaded
  getenv :: y -> [String]

  -- | Returns the credentials that are used to perform the request.
  credentials :: (MonadIO m,HttpClient m) => y -> Security -> OAuthMonad m ()
  
  -- | Given an statement executes the query on yql. This function is
  -- able to decide whenever that query requires authentication
  -- [TODO]. If it does, it uses the oauth token in order to fullfil
  -- the request.
  execute :: (MonadIO m,HttpClient m) => y -> Database -> Expression -> OutputT m String
  execute y db stmt = do mkRequest'  <- fmap execBefore_ (ld' db stmt)
                         mkResponse  <- fmap execAfter_ (ld' db stmt)
                         mkOutput    <- fmap execTransform_ (ld' db stmt)
                         tableDesc   <- descTablesIn y (R.find (=="env") . R.qString . mkRequest' $ emptyRequest) stmt
                         response    <- lift (runOAuth $ do credentials y (security tableDesc)
                                                            serviceRequest HMACSHA1 (Just "yahooapis.com") (mkRequest' $ (myRequest tableDesc) { R.host = fst (endpoint y), R.port = snd (endpoint y) } ))
                         return $ mkOutput (toString (mkResponse response))

    where myRequest = mkRequest stmt (getenv y)

toString :: Response -> String
toString resp | statusOk && isXML = xmlPrint . fromJust . xmlParse $ payload
              | isXML             = xmlPrint . fromJust . xmlParse $ payload
              | otherwise         = payload
  where statusOk = status resp `elem` [200..299]

        payload = U.decode . B.unpack . rspPayload $ resp

        contentType = R.find (\s -> map toLower s == "content-type") (rspHeaders resp)

        isXML = case contentType
                of (x:_) -> "text/xml" `L.isPrefixOf` dropWhile isSpace x
                   _     -> False

instance SessionMgr a => Yql (Backend a) where
  endpoint be = myEndpoint be
  
  setenv be e = be { defaultEnv = e : defaultEnv be }
  
  unsetenv be e = be { defaultEnv = L.delete e (defaultEnv be) }
  
  getenv = defaultEnv
  
  credentials _ Any   = putToken (TwoLegg (Application "no_ckey" "no_csec" OOB) R.empty)
  credentials be App  = ignite (application be)
  credentials be User = do token_ <- liftIO (load (sessionMgr be))
                           reqUrl <- liftIO $ fmap (fromJust . R.parseURL) (tryUsrCfg "oauth_reqtoken_url" defReqUrl)
                           accUrl <- liftIO $ fmap (fromJust . R.parseURL) (tryUsrCfg "oauth_acctoken_url" defAccUrl)
                           case token_
                             of Nothing               -> do ignite (application be) 
                                                            oauthRequest PLAINTEXT Nothing reqUrl
                                                            cliAskAuthorization authUrl
                                                            oauthRequest PLAINTEXT Nothing accUrl
                                                            getToken >>= liftIO . save (sessionMgr be)
                                Just token
                                  | threeLegged token -> do putToken token
                                                            now    <- liftIO getCurrentTime
                                                            offset <- liftIO $ fmap fromJust (mtime (sessionMgr be))
                                                            when (now >= expiration offset token) $
                                                              do oauthRequest PLAINTEXT Nothing accUrl
                                                                 getToken >>=  liftIO . save (sessionMgr be)
                                  | otherwise         -> do putToken token
                                                            oauthRequest PLAINTEXT Nothing reqUrl
                                                            cliAskAuthorization authUrl
                                                            oauthRequest PLAINTEXT Nothing accUrl
                                                            getToken >>= liftIO . save (sessionMgr be)
    where defReqUrl  = "https://api.login.yahoo.com/oauth/v2/get_request_token"
          defAccUrl  = "https://api.login.yahoo.com/oauth/v2/get_token"

          authUrl = head . R.find (=="xoauth_request_auth_url") . oauthParams

          expiration offset token = let timeout = read . R.findWithDefault ("oauth_expires_in","3600") 
                                                       . oauthParams $ token
                                    in addUTCTime (fromInteger timeout) offset

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
