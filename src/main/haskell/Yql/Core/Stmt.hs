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
         Table(..)
       , Column(..)
       , Value(..)
       , Where(..)
       , Statement(..)
       ) where

import Yql.Core.Parser
import Data.List
import Data.Maybe

data Table = Table String

data Column = Column String
            | All

data Value = TxtValue String
           | NumValue String
           | MeValue

data Where = Column `OpEq` Value
           | Column `OpIn` [Value]
           | Where `OpAnd` Where
           | Where `OpOr` Where

data Statement = SELECT [Column] Table (Maybe Where)

-- | Listen to parser events to build Statement type.
stmtBuilder :: ParserEvents Column Table Value Where Statement
stmtBuilder = ParserEvents { onTable    = Table
                           , onColumn   = mkColumn
                           , onTxtValue = TxtValue
                           , onNumValue = NumValue
                           , onMeValue  = MeValue
                           , onSelect   = SELECT
                           , onUpdate   = undefined
                           , onDelete   = undefined
                           , onInsert   = undefined
                           , onDesc     = undefined
                           , onEqExpr   = OpEq
                           , onInExpr   = OpIn
                           , onAndExpr  = OpAnd
                           , onOrExpr   = OpOr
                           }
  where mkColumn "*" = All
        mkColumn x   = Column x

instance Read Statement where
  readsPrec _ input = case (parseYql input stmtBuilder)
                      of Left  _    -> []
                         Right stmt -> [(stmt,"")]

instance Show Statement where
  showsPrec _ (SELECT cols table whre) = showString $ "SELECT " 
                                         ++ intercalate "," (map show cols)
                                         ++ " FROM "
                                         ++ show table
                                         ++ fromMaybe "" (fmap ((" WHERE "++).show) whre)
                                         ++ ";"

instance Show Table where
  showsPrec _ (Table t) = showString t

instance Show Column where
  showsPrec _ (All)      = showString "*"
  showsPrec _ (Column c) = showString c

instance Show Where where
  showsPrec _ (c `OpEq` v)  = showString $ show c ++"="++ show v
  showsPrec _ (c `OpIn` vs) = showString $ show c ++"= ("++ intercalate "," (map show vs) ++")"
  showsPrec _ (l `OpAnd` r) = showString $ show l ++" AND "++ show r
  showsPrec _ (l `OpOr` r)  = showString $ show l ++" OR "++ show r

instance Show Value where
  showsPrec _ (MeValue)    = showString "me"
  showsPrec _ (NumValue v) = showString v
  showsPrec _ (TxtValue v) = showString ("\""++ escape v ++"\"")
    where escape ('"':xs) = '\\' : '"' : escape xs
          escape (x:xs)   = x : escape xs
          escape []       = []