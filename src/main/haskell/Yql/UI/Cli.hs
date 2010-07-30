{-# OPTIONS_GHC -W -Wall -fno-warn-unused-do-bind #-}
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

import Data.Char
import System.Console.Haskeline
import Network.OAuth.Http.HttpClient
import Network.OAuth.Http.Response
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.Trans
import Yql.Core.Backend
import Yql.Core.Parser
import Yql.Core.Stmt
import Yql.Version

eol :: String -> Bool
eol (x:xs) | x==';'    = all isSpace xs
           | otherwise = eol xs
eol []                 = False

outputVersion :: InputT IO ()
outputVersion = outputStrLn $ "iyql version " ++ version

outputLicense :: InputT IO ()
outputLicense = outputStrLn "This is free software. Enter .license to read it"

outputHelp :: InputT IO ()
outputHelp = do outputStrLn "Enter .help for instructions"
                outputStrLn "Enter YQL statements terminated with a \";\""

cont :: String -> InputT IO String
cont prefix = do minput <- getInputLine " ...> "
                 case minput
                   of Nothing                -> return prefix
                      Just input | eol input -> return (prefix ++" "++ input)
                                 | otherwise -> cont (prefix ++" "++ input)

loop :: Yql y => y -> InputT IO ()
loop y = do minput <- getInputLine "iyql> "
            case minput
              of Nothing      -> return ()
                 Just ":quit" -> return ()
                 Just input | eol input  -> do execYql input
                                               loop y
                            | otherwise  -> do (cont input >>= execYql)
                                               loop y

  where empty = all (==' ')
        
        execYql input = case (parseYql input builder)
                        of Left err 
                             | empty input -> loop y
                             | otherwise   -> do outputStrLn (show err)
                                                 outputStrLn ""
                           Right stmt      -> do rsp <- liftIO (fmap rspPayload (unCurlM (execute y stmt)))
                                                 outputStrLn (B.unpack rsp)

iyql :: Yql y => y -> InputT IO ()
iyql y = do outputVersion
            outputLicense
            outputHelp
            loop y
