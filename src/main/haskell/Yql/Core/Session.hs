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

module Yql.Core.Session 
       ( -- * Types
         SessionMgr(..)
       , SessionBackend(..)
       ) where

import Network.OAuth.Consumer
import Data.Time
import Data.Binary
import System.Locale
import System.Directory
import Control.Monad (join,when)

data SessionBackend = FileStorage FilePath
                    | DevNullStorage

-- | Extends the token adding time information.
newtype TimedToken = TimedToken { unToken :: (String,Token) }

augument :: Token -> IO TimedToken 
augument tk = do ts <- fmap (formatTime defaultTimeLocale "%s") getCurrentTime
                 return (TimedToken (ts,tk))

fileSessionSave :: FilePath -> TimedToken -> IO ()
fileSessionSave = encodeFile

fileSessionLoad :: FilePath -> IO (Maybe TimedToken)
fileSessionLoad file = do shouldTry <- doesFileExist file
                          if (shouldTry)
                            then fmap Just (decodeFile file)
                            else return Nothing

-- | Provide support for saving and restoring oauth tokens.
class SessionMgr s where
  -- | Saves the token.
  save :: s -> Token -> IO ()
  
  -- | Loads the latest saved token.
  load :: s -> IO (Maybe Token)
  
  -- | Delete the latest saved token.
  unlink :: s -> IO ()
  
  -- | Returns the time of the latest successfully save operation.
  mtime :: s -> IO (Maybe UTCTime)

instance SessionMgr SessionBackend where
  save (FileStorage file) tk = augument tk >>= fileSessionSave file
  save DevNullStorage _ = return ()
  
  unlink (FileStorage file) = do shouldRemove <- doesFileExist file 
                                 when shouldRemove (removeFile file)
  unlink DevNullStorage     = return ()
  
  load (FileStorage file)    = fmap (fmap (snd . unToken)) (fileSessionLoad file)
  load DevNullStorage  = return Nothing
  
  mtime DevNullStorage = return Nothing
  mtime (FileStorage file)   = fmap (join . fmap (toUTC . fst . unToken)) (fileSessionLoad file)
    where toUTC = parseTime defaultTimeLocale "%s"
  
instance Binary TimedToken where
  put (TimedToken (ts,tk)) = do put ts
                                put tk
  
  get = do ts <- get
           tk <- get
           return (TimedToken (ts,tk))
