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
import Yql.Core.LocalFunction
import Yql.Core.LocalFunctions.Request
import Yql.Core.LocalFunctions.Endpoint
import Yql.Core.LocalFunctions.Tables
import Yql.Core.Session
import Yql.Core.Parser
import Yql.Core.Types
import Yql.UI.CLI.Input
import Yql.UI.CLI.Command
import Yql.UI.CLI.Commands.Parser
import Yql.UI.CLI.Commands.WhoAmI
import Yql.UI.CLI.Commands.Logout
import Yql.UI.CLI.Commands.Login
import qualified Yql.UI.CLI.Commands.SetEnv as E
import qualified Data.Map as M
import qualified Yql.UI.CLI.Options as O

funcDB :: Yql.Core.LocalFunction.Database
funcDB = M.fromList [ ("request", yqlRequest)
                    , ("json", const (yqlRequest [("format",TxtValue "json")]))
                    , ("diagnostics", const (yqlRequest [("diagnostics",TxtValue "true")]))
                    , ("tables", tablesTransform)
                    , ("endpoint", yqlEndpoint)
                    ]

cmdDB :: (SessionMgr s, Yql y) => s -> y -> Yql.UI.CLI.Command.Database y
cmdDB s y = M.insert "help" (bind y $ dump $ help woHelp) woHelp
  where woHelp = M.fromList [ ("logout", bind y $ logout s)
                            , ("login", bind y $ login y)
                            , ("whoami", bind y $ dump $ whoami s)
                            , ("help", bind y $ help M.empty)
                            , ("env", fixSetenv (E.setenv y))
                            ]
        
        fixSetenv (Command (d,f)) = Command (d,proxy)
          where proxy n argv = do output <- f n argv
                                  case output
                                    of Left out -> do putStrLn out
                                                      return y
                                       Right y' -> return y'

outputVersion :: InputT IO ()
outputVersion = outputStrLn $ "iyql version " ++ showVersion version

outputHelp :: InputT IO ()
outputHelp = do outputStrLn "Enter :help for instructions"
                outputStrLn "Enter YQL statements terminated with a \";\""

execYql :: Yql y => y -> String -> InputT IO ()
execYql y input = case (parseYql input builder)
                  of Left err   -> outputStrLn (show err)
                     Right stmt -> fmap (either id id) (liftIO (unCurlM (unOutputT (execute y funcDB stmt)))) >>= outputStrLn

execCmd :: (SessionMgr s,Yql y) => s -> y -> String -> InputT IO (Maybe y)
execCmd s y input = case (parseCmd input)
                    of Nothing              -> do outputStrLn (input ++ " : parse error")
                                                  return (Just y)
                       Just ("quit",_)      -> return Nothing
                       Just (link,argv)     -> case (M.lookup link (cmdDB s y))
                                               of Nothing  -> do outputStrLn (input ++ " : unknown command") 
                                                                 return (Just y)
                                                  Just cmd -> fmap Just (liftIO $ exec cmd link argv)

putenv :: Yql y => y -> [String] -> y
putenv = foldr (flip setenv)

run :: (SessionMgr s,Yql y) => s -> y -> InputT IO ()
run s y = do argv <- liftIO getArgs
             case (O.getoptions argv)
               of Left errors   -> outputStrLn errors
                  Right actions -> runActions argv actions
  where runActions argv opts
          | O.wantVersion opts  = outputVersion
          | O.wantHelp opts     = outputStrLn $ O.usage argv
          | O.wantExecStmt opts = let O.ExecStmt stmt = head . filter O.execStmt $ opts
                                  in execYql y stmt
          | otherwise           = let newY = putenv y (map (\(O.Env e) -> e) (filter O.env opts))
                                  in do outputVersion
                                        outputHelp
                                        loop newY (Handler (execCmd s) execYql)
                                        return ()

iyql :: (SessionMgr s,Yql y) => s -> y -> IO ()
iyql s y = do myCfg  <- fmap settings basedir
              runInputT myCfg (run s y)
  where settings home = Settings { complete       = noCompletion
                                 , historyFile    = Just $ joinPath [home,"history"]
                                 , autoAddHistory = False
                                 }
