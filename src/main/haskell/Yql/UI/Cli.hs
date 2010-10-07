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
import Control.Monad.Trans
import Yql.Data.Version
import Yql.Data.Cfg (basedir,tryUsrCfg)
import Yql.Data.Xml
import Yql.Data.PPrint
import Yql.Data.Trie
import Yql.Core.Backend
import Yql.Core.LocalFunction
import Yql.Core.Lexer (keywords)
import qualified Yql.Core.LocalFunctions.Request as F1
import qualified Yql.Core.LocalFunctions.Tree as F2
import qualified Yql.Core.LocalFunctions.Exec as F3
import Yql.Core.Session
import Yql.Core.Parser
import Yql.Core.Types
import Yql.UI.CLI.Input
import Yql.UI.CLI.Command
import Yql.UI.CLI.Commands.Parser
import Yql.UI.CLI.Commands.WhoAmI
import Yql.UI.CLI.Commands.Logout
import Yql.UI.CLI.Commands.Login
import Yql.UI.CLI.Commands.ManLf
import qualified Yql.UI.CLI.Commands.SetEnv as E
import qualified Data.Map as M
import qualified Yql.UI.CLI.Options as O

funcDBM :: IO Yql.Core.LocalFunction.Database
funcDBM = do { colors <- fmap (=="true") (tryUsrCfg "tree.colors" "false")
             ; return $ M.fromList [ ("request", F1.function)
                                   , ("json", F1.jsonFunction)
                                   , ("diagnostics", F1.diagnosticsFunction)
                                   , ("endpoint", F1.endpointFunction)
                                   , ("tree", F2.function (myRender colors))
                                   , ("exec", F3.function)
                                   ]
             }
  where myRender True  = renderTo Terminal
        myRender False = renderTo Memory

cmdDBM :: (SessionMgr s, Yql y) => s -> y -> IO (Yql.UI.CLI.Command.Database y)
cmdDBM s y = do { woHelp <- woHelpM
                ; return $ M.insert "help" (bind y $ dump $ help woHelp) woHelp
                }
  where woHelpM = do { funcDB <- funcDBM
                     ; return $ M.fromList [ ("logout", bind y $ logout s)
                                           , ("login", bind y $ login y)
                                           , ("whoami", bind y $ dump $ whoami s)
                                           , ("help", bind y $ help M.empty)
                                           , ("env", fixSetenv (E.setenv y))
                                           , ("man", bind y $ dump $ manlf funcDB)
                                           ]
                     }
        
        fixSetenv (Command (d,f)) = Command (d,proxy)
          where proxy n argv = do { output <- f n argv
                                  ; case output
                                    of Left out -> do { putStrLn out
                                                      ; return y
                                                      }
                                       Right y' -> return y'
                                  }

completeCli :: Trie Char -> String -> [Completion]
completeCli t w 
  | member w t = map (\s -> Completion s s True) (w : list)
  | otherwise  = map (\s -> Completion s s True) list
    where fixPrefix = map (w++)
          list      = fixPrefix (toList (subtrie w t))

outputVersion :: String -> InputT IO ()
outputVersion link = outputStrLn $ unlines [ link ++" "++ showVersion version
                                           , "Copyright (C) 2010 dsouza <dsouza+iyql at bitforest.org>"
                                           , "License GPLv3+: <http://github.com/dsouza/iyql/raw/master/LICENSE>"
                                           , "This is free software, and you are welcome to change and redistribute it."
                                           , "This program comes with ABSOLUTELY NO WARRANTY."
                                           ]

outputHelp :: InputT IO ()
outputHelp = do { outputStrLn "Enter :help for instructions"
                ; outputStrLn "Enter YQL statements terminated with a \";\""
                }

execYql_ :: Yql y => y -> String -> InputT IO ()
execYql_ y input = liftIO (execYql y input) >>= outputStrLn

execYql :: Yql y => y -> String -> IO String
execYql y input = do { funcDB <- funcDBM
                     ; case (parseYql input builder)
                       of Left err   -> return (show err)
                          Right stmt -> fmap (either id id) (unOutputT (execute y funcDB stmt))
                     } 
execCmd :: (SessionMgr s,Yql y) => s -> y -> String -> InputT IO (Maybe y)
execCmd s y input = case (parseCmd input)
                    of Nothing              -> do { outputStrLn (input ++ " : parse error")
                                                  ; return (Just y)
                                                  }
                       Just ("quit",_)      -> return Nothing
                       Just (link,argv)     -> do { cmdDB <- liftIO (cmdDBM s y)
                                                  ; case (M.lookup link cmdDB)
                                                    of Nothing  -> do { outputStrLn (input ++ " : unknown command") 
                                                                      ; return (Just y)
                                                                      }
                                                       Just cmd -> fmap Just (liftIO $ bin cmd link argv)
                                                  }

putenv :: Yql y => y -> [String] -> y
putenv = foldr (flip setenv)

putenvM :: Yql y => y -> IO y
putenvM y = do { argv <- getArgs
               ; case (O.getoptions argv)
                 of Right opts -> return (putenv y (map (\(O.Env e) -> e) (filter O.env opts)))
                    _          -> return y
               }
                 
run :: (SessionMgr s,Yql y) => s -> y -> InputT IO ()
run s y = do { argv <- liftIO getArgs
             ; case (O.getoptions argv)
               of Left errors   -> outputStrLn errors
                  Right actions -> runActions argv actions
             }
  where runActions argv opts
          | O.wantVersion opts  = liftIO getProgName >>= outputVersion
          | O.wantHelp opts     = outputStrLn $ O.usage argv
          | O.wantExecStmt opts = let O.ExecStmt stmt = head . filter O.execStmt $ opts
                                  in execYql_ y stmt
          | otherwise           = do { outputHelp
                                     ; loop y (Handler (execCmd s) execYql_)
                                     ; return ()
                                     }


runShowTables :: Yql y => y -> IO [String]
runShowTables y = do { mxml <- fmap xmlParse (execYql y "SHOW TABLES;")
                     ; case mxml
                       of Nothing  -> return []
                          Just xml -> return (readShowTablesXml xml)
                     }

iyql :: (SessionMgr s,Yql y) => s -> y -> IO ()
iyql s y0 = do { y         <- putenvM y0
               ; alltables <- runShowTables y
               ; cmdDB     <- cmdDBM s y0
               ; funcDB    <- funcDBM
               ; myCfg     <- fmap (settings (mkTrie cmdDB funcDB alltables)) basedir
               ; runInputT myCfg (run s y)
               }
  where settings trie home = Settings { complete         = let func = return . completeCli trie
                                                           in completeQuotedWord (Just '\\') "\"'" func (completeWord (Just '\\') " \t;" func)
                                      , historyFile    = Just $ joinPath [home,"history"]
                                      , autoAddHistory = False
                                      }

        mkTrie cmdDB funcDB alltables = fromList . concat $ [ allcommands
                                                            , allfunctions
                                                            , keywords
                                                            , alltables
                                                            ]
          where allcommands  = map (':':) (M.keys cmdDB)
                allfunctions = map ('.':) (M.keys funcDB)
