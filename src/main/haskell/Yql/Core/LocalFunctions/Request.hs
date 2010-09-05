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

module Yql.Core.LocalFunctions.Request
       ( function
       , jsonFunction
       , diagnosticsFunction
       , endpointFunction
       ) where

import Yql.Core.Types
import Yql.Core.LocalFunction
import Network.OAuth.Http.Request as R

function :: Exec
function = Before doc func
  where doc link = unlines [ "Modify the request query string"
                           , "Example:"
                           , "  " ++ link ++ "(format=\"json\")"
                           , "  " ++ link ++ "(diagnostics=\"true\")"
                           , "  " ++ link ++ "(_maxage=3600)"
                           ]
        
        func vs r = r { qString = foldr R.insert (qString r) params }
          where params = map (\(k,v) -> (k,myShow v)) vs
                
                myShow (TxtValue v) = v
                myShow v            = show v

jsonFunction :: Exec
jsonFunction = Before doc func
  where doc _ = unlines [ "Changes the output format to json."
                        ]
        
        func _ r = r { qString = R.insert ("format","json") (qString r) }

diagnosticsFunction :: Exec
diagnosticsFunction = Before doc func
  where doc _ = unlines [ "Sends diagnostics=true parameter to yql"
                        ]
        
        func _ r = r { qString = R.insert ("diagnostics","true") (qString r) }

endpointFunction :: Exec
endpointFunction = Before doc func
  where doc link = unlines [ "Allow you to change the yql endpoint"
                           , "Examples:"
                           , " SELECT * FROM foobar | " ++ link ++ "(host=\"query.yahooapis.com\", port=80);"
                           ]
        
        func vs r = r { host = newHost (host r)
                      , port = newPort (port r)
                      }
                    
          where newHost d = case (lookup "host" vs)
                            of (Just (TxtValue h)) -> h
                               _                   -> d

                newPort d = case (lookup "port" vs)
                            of (Just (NumValue p)) -> read p
                               _                   -> d
