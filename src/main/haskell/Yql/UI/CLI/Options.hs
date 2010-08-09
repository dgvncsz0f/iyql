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

module Yql.UI.CLI.Options 
       ( Options(..)
       , options
       , usage
       , getoptions
       , wantVersion
       , wantHelp
       , wantExecStmt
       , wantVerbose
       , version
       , verbose
       , help
       , execStmt
       ) where

import System.Console.GetOpt

data Options = Verbose
             | Version
             | Help
             | ExecStmt String
             deriving (Eq,Show)

options :: [ OptDescr Options ]
options = [ Option "e" ["execute"] (ReqArg ExecStmt "<stmt>") "Specifies the statement to be sent to yql and print the results to the standard output"
          , Option "V" ["version"] (NoArg Version)            "Print a version number to the standard output"
          -- , Option "v" ["verbose"] (NoArg Verbose)            "Print debug information"
          , Option "h" ["help"]    (NoArg Help)               "This message"
          ]

usage :: [String] -> String
usage argv | null argv = usageInfo "Usage:" options
           | otherwise = usageInfo ("iyql usage:") options

version :: Options -> Bool
version Version = True
version _       = False

verbose :: Options -> Bool
verbose Verbose = True
verbose _       = False

help :: Options -> Bool
help Help = True
help _    = False

execStmt :: Options -> Bool
execStmt (ExecStmt _) = True
execStmt _            = False

wantVersion :: [Options] -> Bool
wantVersion = any version

wantHelp :: [Options] -> Bool
wantHelp = any help

wantVerbose :: [Options] -> Bool
wantVerbose = any version

wantExecStmt :: [Options] -> Bool
wantExecStmt = any execStmt

getoptions :: [String] -> Either String [Options]
getoptions argv = case (errors,unknown)
                  of ([],[]) -> Right actions
                     _       -> Left (concat errors 
                                      ++ concat unknown
                                      ++ usage argv)
  where (actions,unknown,errors) = getOpt Permute options argv
