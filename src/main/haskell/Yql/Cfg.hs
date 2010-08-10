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
         -- * Reading
         cfg 
       ) where

import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce
import System.Directory
import System.FilePath

-- | Read a config entry from the configuration file.
cfg :: String -> a -> IO a
cfg key def = do basedir   <- getHomeDirectory
                 let cfghs = (joinPath [basedir,".iyql/cfg.hs"])
                 available <- doesFileExist cfghs
                 if (available)
                   then defaultErrorHandler defaultDynFlags $ do 
                     val <- runGhc (Just libdir) $ do
                       dflags <- getSessionDynFlags
                       setSessionDynFlags dflags
                       target <- guessTarget "/home/dsouza/.iyql/cfg.hs" Nothing
                       addTarget target
                       r <- load LoadAllTargets
                       case r of
                         Failed -> return def
                         Succeeded -> do
                           m <- findModule (mkModuleName "Yql.User.Cfg") Nothing
                           setContext [] [m]
                           fmap unsafeCoerce (compileExpr ("Yql.User.Cfg." ++ key))
                     return val
                   else return def