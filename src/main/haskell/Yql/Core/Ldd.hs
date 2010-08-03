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

module Yql.Core.Ldd
       ( ldd
       ) where

import Yql.Core.Stmt
import Network.OAuth.Http.Request
-- import Text.Xml.Light

-- | Change the format parameter
yqlRequest :: [(String,Value)] -> Maybe Exec
yqlRequest vs = Just (Before func)
  where myShow (TxtValue v) = v
        myShow v            = show v
        
        params = map (\(k,v) -> (k,myShow v)) vs
        
        func r = r { qString = replaces params (qString r) }

-- -- | Transforms a xml ouput into table format
-- yqlTable :: [(String,Value)] -> Maybe Exec
-- yqlTable _ = Transform $ \xml -> case (fmap table (parseXMLDoc xml))
--                                  of Nothing -> xml
--                                     Just t  -> t
--   where table doc = do results <- fmap (onlyElems . elContent) (findElement (QString "results" Nothing Nothing) doc)
        
--         toTable elem = 

-- | Default linker
ldd :: [(String,[(String,Value)] -> Maybe Exec)]
ldd = [ ("request",yqlRequest)
      , ("json",const (yqlRequest [("format",TxtValue "json")]))
      , ("diagnostics",const (yqlRequest [("diagnostics",TxtValue "true")]))
      ]
