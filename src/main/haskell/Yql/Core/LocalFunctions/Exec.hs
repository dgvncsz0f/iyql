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

module Yql.Core.LocalFunctions.Exec
       ( function
       ) where

import Yql.Core.Types
import System.Process
import Yql.Core.LocalFunction
import Control.Exception as C
import Control.Concurrent
import Control.Monad
import System.Directory
import System.IO
import System.Exit

exec :: String -> [String] -> String -> IO String
exec bin argv input = do { proceed <- binaryOk
                         ; if (proceed)
                           then runExec
                           else return $ "error: could not find/execute file ["++ bin ++"]"
                         }
  where binaryOk = do { found <- doesFileExist bin
                      ; if (found)
                        then getPermissions bin >>= return . executable
                        else return False
                      }

        runExec = do { (Just inh,Just outh,_,pid) <- createProcess (proc bin argv) { std_in  = CreatePipe 
                                                                                   , std_out = CreatePipe
                                                                                   , std_err = Inherit
                                                                                   }
                     ; output  <- hGetContents outh
                     ; outMVar <- newEmptyMVar
                     ; forkIO $ C.evaluate (length output) >> putMVar outMVar ()
                       
                     ; when (not (null input)) $ do { hPutStr inh input
                                                    ; hFlush inh
                                                    }
                     ; hClose inh
                       
                     ; takeMVar outMVar
                     ; hClose outh
                       
                     ; ex <- waitForProcess pid
                     ; case ex
                       of ExitSuccess       -> return output
                          ExitFailure errno -> return ("error running program ["++ bin ++"]. exit " ++ show errno)
                     }

function :: Exec
function = TransformM doc runExec
  where doc _ = unlines [ "Invoke an arbitrary function from the filesystem."
                        , "The program should be able to read input from stdin and write output to stdout. Only stdout is collected for further processing while stderr is printed as-is."
                        , "Examples:"
                        , "  SELECT * FROM social.profile WHERE guid=me | .exec(file=\"/bin/foobar\");"
                        , "  SELECT * FROM social.profile WHERE guid=me | .exec(file=\"/bin/foobar\", argv='[\"foobar\", \"foo bar\"]');"
                        , ""
                        , "N.B.: The reason argv parameter is cumbersome is because it uses read function, which requires valid haskell syntax. In other words, you need to put brackets and use double quotes to every parameter."
                        ]

        runExec argv input = case (lookup "file" argv)
                             of Just (TxtValue bin) -> let computation = exec bin arguments input
                                                           handler     = \(SomeException e) -> return (show e)
                                                       in C.catch computation handler
                                Just _              -> return $ "error: missing file argument"
                                _                   -> return $ "error: missing file argument"
          where arguments = case (lookup "argv" argv)
                            of Just (TxtValue raw) -> case (reads raw)
                                                      of [(arglist,"")] -> arglist
                                                         _              -> []
                               Just _              -> []
                               Nothing             -> []

