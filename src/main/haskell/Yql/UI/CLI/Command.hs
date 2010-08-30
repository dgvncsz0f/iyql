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

module Yql.UI.CLI.Command
       ( Command(..)
       , Database
       , man
       , exec
       , help
       , bind
       , dump
       ) where

import qualified Data.Map as M

-- | The command is a pair consisting of a) The documentation; and b)
-- the command itself.
newtype Command a = Command { runCommand :: (String -> String,String -> [String] -> IO a) }

-- | The database of available commands
type Database a = M.Map String (Command a)

dump :: Command String -> Command ()
dump (Command (d,f)) = Command (d,proxy)
  where proxy n argv = f n argv >>= putStrLn

bind :: a -> Command b -> Command a
bind a (Command (d,f)) = Command (d,proxy)
  where proxy n argv = f n argv >> return a

-- | Extracts the help message of the command
man :: Command a -> String -> String
man = fst . runCommand

-- | Extracts the binary
exec :: Command a -> String -> [String] -> IO a
exec = snd . runCommand

-- | The help command, whith displays all available commands in the database
help :: Database a -> Command String
help database = Command (const doc,const (const exe))
  where doc = "This message"
        exe = return $ unlines $ map (manThis 16) (M.toList database)
          where manThis avail (link,cmd) = let diff = max 0 (avail - length link)
                                           in " :" ++ link ++ (replicate diff ' ') ++ (addMargin (avail+2) (lines (man cmd link)))
                addMargin by (l:ls) = unlines (l : map ((replicate by ' ')++) ls)
                addMargin _ []      = unlines []

