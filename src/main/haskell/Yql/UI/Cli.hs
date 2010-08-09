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

module Yql.UI.Cli where

import System
import System.Console.Haskeline
import Network.OAuth.Http.HttpClient
import Control.Monad.Trans
import Data.Char
import Yql.Core.Backend
import Yql.Core.Parser
import Yql.Core.Stmt
import Yql.Core.Ldd
import qualified Yql.Version as V
import Yql.UI.CLI.Options

eol :: String -> Bool
eol (x:xs) | x==';'    = all isSpace xs
           | otherwise = eol xs
eol []                 = False

empty :: String -> Bool
empty = all isSpace

outputVersion :: InputT IO ()
outputVersion = outputStrLn $ "iyql version " ++ V.version

outputLicense :: InputT IO ()
outputLicense = outputStrLn "This is free software. Enter .license to read it"

outputHelp :: InputT IO ()
outputHelp = do outputStrLn "Enter .help for instructions"
                outputStrLn "Enter YQL statements terminated with a \";\""

cont :: String -> InputT IO String
cont prefix = do minput <- getInputLine " ...> "
                 case minput
                   of Nothing       -> return prefix
                      Just input 
                        | eol input -> return (prefix ++" "++ input)
                        | otherwise -> cont (prefix ++" "++ input)

execYql :: Yql y => y -> String -> InputT IO String
execYql y input = case (parseYql input builder)
                  of Left err   -> return (show err)
                     Right stmt -> liftIO (unCurlM (unOutputT (execute y ldd stmt))) >>= return . either id id

loop :: Yql y => y -> InputT IO ()
loop y = do minput <- getInputLine "iyql> "
            case minput
              of Nothing         -> return ()
                 Just ":quit"    -> return ()
                 Just input 
                   | empty input -> loop y
                   | eol input   -> do (execYql y input >>= outputStrLn)
                                       loop y
                   | otherwise   -> do (cont input >>= execYql y >>= outputStrLn)
                                       loop y


iyql :: Yql y => y -> InputT IO ()
iyql y = do argv <- liftIO getArgs
            case (getoptions argv)
              of Left errors   -> outputStrLn errors
                 Right actions -> runActions argv actions
  where runActions argv opts 
          | wantVersion opts  = outputVersion
          | wantHelp opts     = outputStrLn $ usage argv
          | wantExecStmt opts = let ExecStmt stmt = head . filter execStmt $ opts
                                in execYql y stmt >>= outputStrLn
          | otherwise         = do outputVersion
                                   outputLicense
                                   outputHelp
                                   loop y
