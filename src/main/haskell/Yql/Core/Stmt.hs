{-# OPTIONS_GHC -W -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances #-}
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
       , Exec(..)
       , Pipeline(..)
       , Linker(..)
         -- * Query
       , select
       , local
       , remote
       , functions
         -- * Parsing
       , builder 
         -- * Priting
       , showStmt
       , showFunc
       , showValue
       , showWhere
         -- * Misc
       , resolve
       , pipeline
       , ldd
       ) where

import Yql.Core.Parser
import Data.List
import Data.Maybe
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Control.Monad

-- | The different type of values that may appear in a yql statement.
data Value = TxtValue String
           | NumValue String
           | MeValue
           deriving (Eq)

-- | Where clause to filter/limit data in yql statements.
data Where = String `OpEq` Value
           | String `OpIn` [Value]
           | Where `OpAnd` Where
           | Where `OpOr` Where
           deriving (Eq)

-- | Functions that transform output.
data Function = Local { name :: String
                      , args :: [(String,Value)]
                      }
              | Remote { name :: String
                       , args :: [(String,Value)]
                       }
              deriving (Eq)

-- | The different statements supported.
data Statement = SELECT [String] String (Maybe Where) [Function]
               deriving (Eq)

-- | Local functions that may change a given yql query
data Exec = Before (Request -> Request)
          | After (Response -> Response)
          | Transform (String -> String)
          | Seq Exec Exec

-- | Sequence a group of Exec types in order to make it easier to
-- execute.
data Pipeline = ExecTransform { before    :: Request -> Request
                              , after     :: Response -> Response
                              , transform :: String -> String
                              }

-- | Database of exec types.
class Linker r where
  link :: r -> String -> [(String,Value)] -> Maybe Exec

-- | Transforms a list of functions into a pipeline using a given linker.
pipeline :: Monad m => Linker l => l -> [Function] -> m Pipeline
pipeline _ []     = return (ExecTransform id id id)
pipeline l (f:fs) = case (link l (name f) (args f))
                    of Nothing -> fail $ "unknown function: " ++ name f
                       Just ex -> liftM (merge ex) (pipeline l fs)
  where merge t0 t = case t0
                     of Before fx    -> t { before    = fx . (before t) }
                        After fx     -> t { after     = fx . (after t) }
                        Transform fx -> t { transform = fx . (transform t) }
                        Seq ex0 ex1  -> merge ex0 (merge ex1 t)

-- | Extracts the local functions from the statement and creates a pipeline.
resolve :: (Monad m, Linker l) => l -> Statement -> m Pipeline
resolve l stmt = let fs = filter local (functions stmt)
                 in pipeline l fs

-- | Change the format parameter
yqlRequest :: [(String,Value)] -> Maybe Exec
yqlRequest vs = Just (Before func)
  where myShow (TxtValue v) = v
        myShow v            = show v
        
        params = map (\(k,v) -> (k,myShow v)) vs
        
        func r = r { qString = replaces params (qString r) }

-- | Default linker
ldd :: [(String,[(String,Value)] -> Maybe Exec)]
ldd = [ ("request",yqlRequest)
      ] 

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
                       , onRemoteFunc = Remote
                       , onLocalFunc  = Local
                       }

-- | Test if the statement is a select statement
select :: Statement -> Bool
select (SELECT _ _ _ _) = True

local :: Function -> Bool
local (Local _ _) = True
local _           = False

remote :: Function -> Bool
remote (Remote _ _) = True
remote _            = False

functions :: Statement -> [Function]
functions (SELECT _ _ _ f) = f

showStmt :: Statement -> String
showStmt(SELECT cols table whre func) = "SELECT " 
                                        ++ intercalate "," cols
                                        ++ " FROM "
                                        ++ table
                                        ++ fromMaybe "" (fmap ((" WHERE "++).show) whre)
                                        ++ funcString
                                        ++ ";"
  where funcString | null func = ""
                   | otherwise = " | " ++ intercalate " | " (map show func)

showFunc :: Function -> String
showFunc f = prefix ++ name f ++ "(" ++ intercalate "," (map showArg (args f)) ++ ")"
    where showArg (k,v) = k ++ "=" ++ show v
          
          prefix | local f   = "."
                 | otherwise = ""

showWhere :: Where -> String
showWhere (c `OpEq` v)  = c ++"="++ show v
showWhere (c `OpIn` vs) = c ++" IN ("++ intercalate "," (map show vs) ++")"
showWhere (l `OpAnd` r) = show l ++" AND "++ show r
showWhere (l `OpOr` r)  = show l ++" OR "++ show r

showValue :: Value -> String
showValue (MeValue)    = "me"
showValue (NumValue v) = v
showValue (TxtValue v) = ("\""++ escape v ++"\"")
  where escape ('"':xs) = '\\' : '"' : escape xs
        escape (x:xs)   = x : escape xs
        escape []       = []

instance Read Statement where
  readsPrec _ input = case (parseYql input builder)
                      of Left  _    -> []
                         Right stmt -> [(stmt,"")]

instance Show Statement where
  showsPrec _ = showString . showStmt

instance Show Function where
  showsPrec _ = showString . showFunc

instance Show Where where
  showsPrec _ = showString . showWhere

instance Show Value where
  showsPrec _ = showString . showValue

instance Linker () where
  link _ _ _ = Nothing

instance Linker (String,[(String,Value)] -> Maybe Exec) where
  link (k0,f) k1 argv | k1==k0    = (f argv)
                      | otherwise = Nothing

instance Linker l => Linker [l] where
  link r k argv = foldr mplus Nothing (zipWith ($) (map (uncurry . link) r) (repeat (k,argv)))