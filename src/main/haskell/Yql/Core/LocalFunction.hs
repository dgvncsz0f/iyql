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
       , Exec(..)
       , pipeline
       , ld'
       , execBefore
       , execAfter
       , execTransform
       ) where

import Yql.Core.Types
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import qualified Data.Map as M
import Control.Monad

type Database = M.Map String ([(String,Value)] -> Exec)

-- | Local functions that may change a given yql query
data Exec = Before (Request -> Request)
          | After (Response -> Response)
          | Transform (String -> String)
          | Seq Exec Exec
          | NOp

-- | Transforms a list of functions into a pipeline using a given linker.
pipeline :: Monad m => Database -> [Function] -> m Exec
pipeline _ []     = return NOp
pipeline db (f:fs) = case (M.lookup (name f) db)
                     of Nothing -> fail $ "unknown function: " ++ name f
                        Just ex -> liftM (ex (args f) `Seq`) (pipeline db fs)

-- | Extracts the local functions from the statement and creates a pipeline.
ld' :: (Monad m) => Database -> Expression -> m Exec
ld' l stmt = let fs = filter local (functions stmt)
             in pipeline l fs

execTransform :: Exec -> String -> String
execTransform (Transform f) s = f s
execTransform (Seq fa fb) s   = execTransform fb (execTransform fa s)
execTransform _ s             = s

execBefore :: Exec -> Request -> Request
execBefore (Before f) r  = f r
execBefore (Seq fa fb) r = execBefore fb (execBefore fa r)
execBefore _ r           = r

execAfter :: Exec -> Response -> Response
execAfter (After f) r   = f r
execAfter (Seq fa fb) r = execAfter fb (execAfter fa r)
execAfter _ r           = r
