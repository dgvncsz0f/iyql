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

module Yql.Core.LocalFunction
       ( Database
       , Pipeline()
       , Exec(..)
       , man
       , pipeline
       , ld'
       , execBefore
       , execAfter
       , execTransformM
       , execBefore_
       , execAfter_
       , execTransformM_
       ) where

import Yql.Core.Types
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import qualified Data.Map as M
import Control.Monad

type Database = M.Map String Exec

newtype Pipeline = Pipeline { runPipeline :: Exec }

-- | Local functions that may change a given yql query
data Exec = Before (String -> String) ([(String,Value)] -> Request -> Request)
          | After (String -> String) ([(String,Value)] -> Response -> Response)
          | Transform (String -> String) ([(String,Value)] -> String -> String)
          | TransformM (String -> String) ([(String,Value)] -> String -> IO String)
          | Seq Exec Exec
          | NOp

-- | The documentation of a given function
man :: Exec -> String -> String
man (Before doc _)     = doc
man (After doc _)      = doc
man (Transform doc _)  = doc
man (TransformM doc _) = doc
man _                  = error "man: not found"

-- | Transforms a list of functions into a pipeline using a given linker.
pipeline :: Monad m => Database -> [Function] -> m Pipeline
pipeline db funcs = do { exec <- pipeline' funcs
                       ; return (Pipeline exec)
                       }
  where pipeline' []     = return NOp
        pipeline' (f:fs) = case (M.lookup (name f) db)
                           of Nothing -> fail $ "unknown function: " ++ name f
                              Just ex -> liftM (bind (args f) ex `Seq`) (pipeline' fs)
        
        bind argv (Before d f)     = Before d (const $ f argv)
        bind argv (After d f)      = After d (const $ f argv)
        bind argv (Transform d f)  = Transform d (const $ f argv)
        bind argv (TransformM d f) = TransformM d (const $ f argv)
        bind _ x                   = x

-- | Extracts the local functions from the statement and creates a pipeline.
ld' :: (Monad m) => Database -> Expression -> m Pipeline
ld' l stmt = let fs = filter local (functions stmt)
             in pipeline l fs

execTransformM :: [(String,Value)] -> Exec -> String -> IO String
execTransformM argv (Transform _ f) s  = return (f argv s)
execTransformM argv (TransformM _ f) s = f argv s
execTransformM argv (Seq fa fb) s      = execTransformM argv fa s >>= execTransformM argv fb
execTransformM _ _ s                   = return s

execTransformM_ :: Pipeline -> String -> IO String
execTransformM_ = execTransformM [] . runPipeline

execBefore :: [(String,Value)] -> Exec -> Request -> Request
execBefore argv (Before _ f) r = f argv r
execBefore argv (Seq fa fb) r  = execBefore argv fb (execBefore argv fa r)
execBefore _ _ r               = r

execBefore_ :: Pipeline -> Request -> Request
execBefore_ = execBefore [] . runPipeline

execAfter :: [(String,Value)] -> Exec -> Response -> Response
execAfter argv (After _ f) r = f argv r
execAfter argv (Seq fa fb) r = execAfter argv fb (execAfter argv fa r)
execAfter _ _ r              = r

execAfter_ :: Pipeline -> Response -> Response
execAfter_ = execAfter [] . runPipeline
