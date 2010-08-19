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

module Yql.Cfg
       ( -- * Types
         Cfg()
         -- * Reading
       , cfg 
       , tryCfg
       , tryUsrCfg
       , usrCfg
       , parseCfg
       , basedir
         -- * Building
       , empty
       , fromList
       ) where

import Prelude hiding (catch)
import Data.Char
import Data.List
import Control.Exception
import System.FilePath
import System.Directory
import System.IO

newtype Cfg = Cfg [(String,String)]
            deriving (Show,Eq,Ord)

-- | Returns an empty configuration
empty :: Cfg
empty = fromList []

-- | Reads configuration from a list
fromList :: [(String,String)] -> Cfg
fromList = Cfg

-- | Read a config entry or return a default value in case there is an
-- error.
tryCfg :: Cfg -> String -> String -> String
tryCfg (Cfg db) key def = case (lookup key db)
                          of Nothing  -> def
                             Just rst -> rst

-- | Read a config from the user configuration file.
tryUsrCfg :: String -> String -> IO String
tryUsrCfg key def = usrCfg >>= \db -> return $ tryCfg db key def

-- | Read a config entry from the configuration file.
cfg :: FilePath -> IO Cfg
cfg file = catch readCfg (\(SomeException _) -> return (Cfg []))
  where readCfg = do available <- doesFileExist file
                     if (available)
                       then bracket (openFile file ReadMode) hClose (\h -> fmap parseCfg (hGetContents h >>= evaluate))
                       else return (Cfg [])

-- | The configuration file is as follows:
--    1. Lines starting with -- are considered comments;
--    2. Empty lines are also discarded;
--    3. Entries are simply pairs, separated by colon (:);
parseCfg :: String -> Cfg
parseCfg = Cfg . map (parseEntry . stripComments) . filter properEntries . lines
  where properEntries xs0 
          | "--" `isPrefixOf` xs = False
          | null xs              = False
          | otherwise            = True
            where xs = dropWhile isSpace xs0
        
        parseEntry xs = let (key,val) = break (==':') (dropWhile isSpace xs)
                        in (key,dropWhile isSpace (drop 1 val))
        
        stripComments []          = []
        stripComments ('-':'-':_) = []
        stripComments (x:xs)      = x : stripComments xs

-- | Base directory where configuration files are read from
basedir :: IO FilePath
basedir = fmap (\home -> joinPath [home,".iyql"]) getHomeDirectory

-- | File configuration file
usrCfg :: IO Cfg
usrCfg = do home <- basedir
            cfg (joinPath [home,"cfg"])
