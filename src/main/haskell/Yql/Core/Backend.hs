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
         -- * Session Util
       , fileSave
       , fileLoad
       ) where

import Yql.Core.Stmt
import Yql.Core.Ldd
import Yql.Xml
import Data.Char
import Data.Maybe
import Data.List (isPrefixOf)
import Data.Time
import Data.Binary
import System.Locale
import System.Directory
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import qualified Codec.Binary.UTF8.String as U
import Network.OAuth.Consumer hiding (application)
import Network.OAuth.Http.Request hiding (insert)
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient

newtype OutputT m a = OutputT { unOutputT :: m (Either String a) }

data Backend = YqlBackend { application :: Application
                          , sessionSave :: Token -> IO ()
                          , sessionLoad :: IO (Maybe Token)
                          }

fileSave :: FilePath -> Token -> IO ()
fileSave = encodeFile

fileLoad :: FilePath -> IO (Maybe Token)
fileLoad file = do valid <- doesFileExist file 
                   if (valid)
                     then fmap Just (decodeFile file)
                     else return Nothing

-- | Tells the minimum security level required to perform this
-- statament.
executeDesc :: (MonadIO m,HttpClient m,Yql y) => y -> String -> String -> OutputT m Description
executeDesc y t env = execute y ldd (DESC t [Local "request" [("env",TxtValue env)]]) >>= parseXml
    where parseXml xml = case (xmlParse xml)
                         of Just doc -> case (readDescXml doc)
                                        of Just rst -> return rst
                                           Nothing  -> fail $ "couldn't desc table " ++ t
                            Nothing  -> fail "error parsing xml"

emptyRequest :: Request
emptyRequest = ReqHttp { version    = Http11
                       , ssl        = False
                       , host       = "query.yahooapis.com"
                       , port       = 80
                       , method     = GET
                       , reqHeaders = empty
                       , pathComps  = ["","v1","public","yql"]
                       , qString    = empty
                       , reqPayload = B.empty
                       }

yqlRequest :: Statement -> Description -> Request
yqlRequest stmt d = emptyRequest { ssl        = https d
                                 , port       = portNumber
                                 , pathComps  = yqlPath
                                 , method     = httpMethod
                                 , qString    = fromList [("q",show preparedStmt),("env","store://datatables.org/alltableswithkeys")]
                                 }
  where yqlPath
          | security d `elem` [User,App] = ["","v1","yql"]
          | otherwise                    = ["","v1","public","yql"]

        portNumber
          | https d   = 443
          | otherwise = 80

        httpMethod | update stmt = PUT
                   | insert stmt = PUT
                   | otherwise   = GET

        preparedStmt = case stmt
                       of (SELECT c t w f) -> SELECT c t w (filter remote f)
                          (DESC t _)       -> DESC t []
                          (UPDATE c t w f) -> UPDATE c t w (filter remote f)
                          (INSERT c t f)   -> INSERT c t (filter remote f)

-- | Minimum complete definition: endpoint, app.
class Yql y where
  -- | Returns the endpoint this backend is pointing to
  endpoint :: y -> String

  -- | Returns the credentials that are used to perform the request.
  credentials :: (MonadIO m,HttpClient m) => y -> Security -> OAuthMonad m ()

  -- | Given an statement executes the query on yql. This function is
  -- able to decide whenever that query requires authentication
  -- [TODO]. If it does, it uses the oauth token in order to fullfil
  -- the request.
  execute :: (MonadIO m,HttpClient m,Linker l) => y -> l -> Statement -> OutputT m String
  execute y l stmt = do mkRequest   <- fmap execBefore (ld l stmt)
                        mkResponse  <- fmap execAfter (ld l stmt)
                        mkOutput    <- fmap execTransform (ld l stmt)
                        tableDesc   <- descTable (findWithDefault ("env","store://datatables.org/alltableswithkeys") . qString . mkRequest $ emptyRequest)
                        response    <- lift (runOAuth $ do credentials y (security tableDesc)
                                                           serviceRequest HMACSHA1 (Just "yahooapis.com") ((mkRequest $ myRequest tableDesc) { host = endpoint y } ))
                        return $ mkOutput (toString (mkResponse response))

    where descTable env = case stmt
                          of _ | desc stmt    -> return $ foldr1 joinDesc (map (const $ Table "<<many>>" Any False) (tables stmt))
                               | usingMe stmt -> return $ foldr1 joinDesc (map (const $ Table "<<many>>" User False) (tables stmt))
                               | otherwise    -> fmap (foldr1 joinDesc) (mapM (flip (executeDesc y) env) (tables stmt))
            where joinDesc (Table _ sec0 ssl0) (Table _ sec1 ssl1) = Table "<<many>>" (max sec0 sec1) (ssl0 || ssl1)

          myRequest = yqlRequest stmt


toString :: Response -> String
toString resp | statusOk && isXML = xmlPrint . fromJust . xmlParse $ payload
              | isXML             = xmlPrint . fromJust . xmlParse $ payload
              | otherwise         = payload
  where statusOk = status resp `elem` [200..299]

        payload = U.decode . B.unpack . rspPayload $ resp

        contentType = find (\s -> map toLower s == "content-type") (rspHeaders resp)

        isXML = case contentType
                of (x:_) -> "text/xml" `isPrefixOf` (dropWhile isSpace x)
                   _     -> False

instance Yql Backend where
  endpoint _ = "query.yahooapis.com"

  credentials _ Any   = putToken (TwoLegg (Application "no_ckey" "no_csec" OOB) empty)
  credentials be App  = ignite (application be)
  credentials be User = do token_ <- liftIO (sessionLoad be)
                           case token_
                             of Nothing               -> do ignite (application be)
                                                            oauthRequest PLAINTEXT Nothing reqUrl
                                                            cliAskAuthorization authUrl
                                                            oauthRequest PLAINTEXT Nothing accUrl
                                                            getToken >>= liftIO . sessionSave be
                                Just token
                                  | threeLegged token -> do putToken token
                                                            now <- liftIO getCurrentTime
                                                            if (expiration token >= now)
                                                              then do oauthRequest PLAINTEXT Nothing accUrl
                                                                      getToken >>=  liftIO . sessionSave be
                                                              else return ()
                                  | otherwise         -> do putToken token
                                                            oauthRequest PLAINTEXT Nothing reqUrl
                                                            cliAskAuthorization authUrl
                                                            oauthRequest PLAINTEXT Nothing accUrl
                                                            getToken >>= liftIO . sessionSave be
    where reqUrl  = fromJust . parseURL $ "https://api.login.yahoo.com/oauth/v2/get_request_token"

          accUrl  = fromJust . parseURL $ "https://api.login.yahoo.com/oauth/v2/get_token"

          authUrl = findWithDefault ("xoauth_request_auth_url",error "xoauth_request_auth_url not found") . oauthParams

          expiration = fromJust
                       . parseTime defaultTimeLocale "%s"
                       . findWithDefault ("oauth_authorization_expires_in","0")
                       . oauthParams

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
