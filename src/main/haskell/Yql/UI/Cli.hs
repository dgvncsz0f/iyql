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
import System.FilePath
import System.Console.Haskeline
import Network.OAuth.Http.HttpClient
import Control.Monad.Trans
import Yql.Version
import Yql.Cfg (basedir)
import Yql.Core.Backend
import Yql.Core.Parser
import Yql.Core.Types
import Yql.Core.Ldd
import Yql.UI.CLI.Input
import Yql.UI.CLI.Command
import qualified Yql.UI.CLI.Options as O

outputVersion :: InputT IO ()
outputVersion = outputStrLn $ "iyql version " ++ showVersion version

outputLicense :: InputT IO ()
outputLicense = outputStrLn "This is free software. Enter .license to read it"

outputHelp :: InputT IO ()
outputHelp = do outputStrLn "Enter .help for instructions"
                outputStrLn "Enter YQL statements terminated with a \";\""

execYql :: Yql y => y -> String -> InputT IO ()
execYql y input = case (parseYql input builder)
                  of Left err   -> outputStrLn (show err)
                     Right stmt -> do output <- liftIO (unCurlM (unOutputT (execute y ldd stmt))) >>= return . either id id
                                      outputStrLn output

execCmd :: Yql y => y -> String -> InputT IO Bool
execCmd _ ":quit"   = return False
execCmd y ":logout" = do liftIO $ runCommand logout y []
                         return True
execCmd y ":whoami" = do liftIO $ runCommand whoami y []
                         return True
execCmd _ cmd       = do outputStrLn (cmd ++ ": command not found")
                         return True

exec :: Yql y => y -> InputT IO ()
exec y = do argv <- liftIO getArgs
            case (O.getoptions argv)
              of Left errors   -> outputStrLn errors
                 Right actions -> runActions argv actions
  where runActions argv opts
          | O.wantVersion opts  = outputVersion
          | O.wantHelp opts     = outputStrLn $ O.usage argv
          | O.wantExecStmt opts = let O.ExecStmt stmt = head . filter O.execStmt $ opts
                                  in execYql y stmt
          | otherwise           = do outputVersion
                                     outputLicense
                                     outputHelp
                                     loop (Handler (execCmd y) (execYql y))
                                     return ()

iyql :: Yql y => y -> IO ()
iyql y = do myCfg  <- fmap settings basedir
            runInputT myCfg (exec y)
  where settings home = Settings { complete       = noCompletion
                                 , historyFile    = Just $ joinPath [home,"history"]
                                 , autoAddHistory = False
                                 }