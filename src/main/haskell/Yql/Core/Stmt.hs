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

module Yql.Core.Stmt
       ( -- * Types
         Value(..)
       , Where(..)
       , Statement(..)
       , Function(..)
         -- * Query
       , select
       , local
       , remote
         -- * Parsing
       , builder 
       ) where

import Yql.Core.Parser
import Data.List
import Data.Maybe

-- | The different type of values that may appear in a yql statement.
data Value = TxtValue String
           | NumValue String
           | MeValue
           deriving (Eq)

-- | The table in yql statements.
-- | Where clause to filter/limit data in yql statements.
data Where = String `OpEq` Value
           | String `OpIn` [Value]
           | Where `OpAnd` Where
           | Where `OpOr` Where
           deriving (Eq)

-- | Functions that transform output.
data Function = Remote { name :: String
                       , args :: [(String,Value)]
                       }
              | Local { name :: String
                      , args :: [(String,Value)]
                      }
              deriving (Eq)

-- | The different statements supported.
data Statement = SELECT [String] String (Maybe Where) [Function]
               deriving (Eq)

-- | Listen to parser events to build Statement type.
builder :: ParserEvents String Value Where Function Statement
builder = ParserEvents { onIdentifier = id
                       , onTxtValue   = TxtValue
                       , onNumValue   = NumValue
                       , onMeValue    = MeValue
                       , onSelect     = SELECT
                       , onUpdate     = undefined
                       , onDelete     = undefined
                       , onInsert     = undefined
                       , onDesc       = undefined
                       , onEqExpr     = OpEq
                       , onInExpr     = OpIn
                       , onAndExpr    = OpAnd
                       , onOrExpr     = OpOr
                       , onFunction   = mkFunc
                       }
  where mkFunc ('.':f) = Local f
        mkFunc f       = Remote f

-- | Test if the statement is a select statement
select :: Statement -> Bool
select (SELECT _ _ _ _) = True

local :: Function -> Bool
local (Local _ _) = True
local _           = False

remote :: Function -> Bool
remote (Remote _ _) = True
remote _            = False

instance Read Statement where
  readsPrec _ input = case (parseYql input builder)
                      of Left  _    -> []
                         Right stmt -> [(stmt,"")]

instance Show Statement where
  showsPrec _ (SELECT cols table whre func) = let stmt = "SELECT " 
                                                         ++ intercalate "," cols
                                                         ++ " FROM "
                                                         ++ table
                                                         ++ fromMaybe "" (fmap ((" WHERE "++).show) whre)
                                                  showFunc = intercalate " | " (map show func)
                                              in case func
                                                 of [] -> showString (stmt ++ ";")
                                                    _  -> showString (stmt ++ " | " ++ showFunc ++ ";")

instance Show Function where
  showsPrec _ f = showString $ prefix ++ name f ++ "(" ++ intercalate "," (map showArg (args f)) ++ ")"
    where showArg (k,v) = k ++ "=" ++ show v

          prefix | local f   = "."
                 | otherwise = ""

instance Show Where where
  showsPrec _ (c `OpEq` v)  = showString $ c ++"="++ show v
  showsPrec _ (c `OpIn` vs) = showString $ c ++" IN ("++ intercalate "," (map show vs) ++")"
  showsPrec _ (l `OpAnd` r) = showString $ show l ++" AND "++ show r
  showsPrec _ (l `OpOr` r)  = showString $ show l ++" OR "++ show r

instance Show Value where
  showsPrec _ (MeValue)    = showString "me"
  showsPrec _ (NumValue v) = showString v
  showsPrec _ (TxtValue v) = showString ("\""++ escape v ++"\"")
    where escape ('"':xs) = '\\' : '"' : escape xs
          escape (x:xs)   = x : escape xs
          escape []       = []