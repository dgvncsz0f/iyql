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

module Yql.UI.CLI.Input
       ( -- * Types
         EventHandler(..)
       , loop
       ) where

import System.Console.Haskeline
import System.Console.Haskeline.History
import Control.Monad.State
import Data.Maybe
import Data.Char
import Data.List
import Yql.Core.Backend

data EventHandler y = Handler { execCommand   :: y -> String -> InputT IO (Maybe y)
                              , execStatement :: y -> String -> InputT IO ()
                              }

empty :: String -> Bool
empty = all isSpace

prompt :: (String,String)
prompt = ("iyql> "," ...> ")

defPrompt :: String
defPrompt = fst prompt

contPrompt :: String
contPrompt = snd prompt

appendHistory :: String -> InputT IO ()
appendHistory e = fmap (addHistoryUnlessConsecutiveDupe e) get >>= put

command :: String -> Bool
command = (":" `isPrefixOf`) . dropWhile isSpace

next :: InputT IO (Maybe String)
next = next_ defPrompt
  where next_ p = do minput <- getInputLine p
                     case minput 
                       of Nothing            -> return Nothing
                          Just input 
                            | empty input    -> next_ defPrompt
                            | scEnding input -> return (Just input)
                            | command input  -> return (Just input)
                            | otherwise      -> do Just suffix <- fmap (`mplus` Just "") (next_ contPrompt)
                                                   return (Just $ input ++" "++ suffix)
        
        scEnding xs = ";" `isPrefixOf` (dropWhile (isSpace) (reverse xs))

loop :: Yql y => y -> EventHandler y -> InputT IO ()
loop y ex = do minput <- next
               case minput
                 of Nothing           -> return ()
                    Just input 
                      | command input -> do appendHistory input
                                            newY <- execCmd input
                                            when (isJust newY) (loop (fromJust newY) ex)
                      | otherwise     -> do appendHistory input
                                            execStmt input
                                            loop y ex
  where execCmd  = execCommand ex y
        execStmt = execStatement ex y
