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
       , Description(..)
       , Security(..)
       , Statement(..)
       , Function(..)
       , Exec(..)
       , Linker(..)
         -- * Query
       , select
       , update
       , insert
       , delete
       , desc
       , local
       , remote
       , functions
       , usingMe
       , tables
         -- * Parsing
       , builder
       , readStmt
       , readDescXml
         -- * Priting
       , showStmt
       , showFunc
       , showValue
       , showWhere
         -- * Misc
       , ld
       , pipeline
       , execBefore
       , execAfter
       , execTransform
       ) where

import Yql.Core.Parser
import Yql.Xml
import Data.List hiding (insert,delete)
import Data.Char
import Network.OAuth.Http.Request hiding (insert,DELETE)
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
               | DESC String [Function]
               | UPDATE [(String,Value)] String (Maybe Where) [Function]
               | INSERT [(String,Value)] String [Function]
               | DELETE String (Maybe Where) [Function]
               deriving (Eq)

-- | Local functions that may change a given yql query
data Exec = Before (Request -> Request)
          | After (Response -> Response)
          | Transform (String -> String)
          | Seq Exec Exec
          | NOp

-- | The different security level tables may request
data Security = User    -- ^ Requires 3-legged oauth to perform the request
              | App     -- ^ Requires 2-legged oauth to perform the request
              | Any     -- ^ No authentication is required
              deriving (Eq)

-- | The description of a table, usually the result of a desc <table>
-- command.
data Description = Table { table    :: String
                         , security :: Security
                         , https    :: Bool
                         }
                 deriving (Eq)

-- | Database of exec types.
class Linker r where
  link :: r -> String -> [(String,Value)] -> Maybe Exec

execTransform :: Exec -> String -> String
execTransform (Transform f) s = f s
execTransform (Seq fa fb) s   = execTransform fb (execTransform fa s)
execTransform _ s             = s

execBefore :: Exec -> Request -> Request
execBefore (Before f) r  = f r
execBefore (Seq fa fb) r = execBefore fb (execBefore fa r)
execBefore _ r           = r

execAfter :: Exec -> Response -> Response
execAfter (After f) r   = f r
execAfter (Seq fa fb) r = execAfter fb (execAfter fa r)
execAfter _ r           = r

-- | Transforms a list of functions into a pipeline using a given linker.
pipeline :: Monad m => Linker l => l -> [Function] -> m Exec
pipeline _ []     = return NOp
pipeline l (f:fs) = case (link l (name f) (args f))
                    of Nothing -> fail $ "unknown function: " ++ name f
                       Just ex -> liftM (ex `Seq`) (pipeline l fs)

-- | Extracts the local functions from the statement and creates a pipeline.
ld :: (Monad m, Linker l) => l -> Statement -> m Exec
ld l stmt = let fs = filter local (functions stmt)
            in pipeline l fs

-- | Listen to parser events to build Statement type.
builder :: ParserEvents String Value Where Function Statement
builder = ParserEvents { onIdentifier = id
                       , onTxtValue   = TxtValue
                       , onNumValue   = NumValue
                       , onMeValue    = MeValue
                       , onSelect     = SELECT
                       , onUpdate     = UPDATE
                       , onDelete     = DELETE
                       , onInsert     = INSERT
                       , onDesc       = DESC
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
select _                = False

-- | Test if the statment is a desc statament
desc :: Statement -> Bool
desc (DESC _ _) = True
desc _          = False

update :: Statement -> Bool
update (UPDATE _ _ _ _) = True
update _                = False

-- | Test if the statement is a insert statement
insert :: Statement -> Bool
insert (INSERT _ _ _) = True
insert _              = False

-- | Test if the statement is a delete statement
delete :: Statement -> Bool
delete (DELETE _ _ _) = True
delete _              = False

-- | Test if the function is a local function
local :: Function -> Bool
local (Local _ _) = True
local _           = False

-- | Test if the function is a remote function
remote :: Function -> Bool
remote (Remote _ _) = True
remote _            = False

-- | Extracts all tables in use in the statement
tables :: Statement -> [String]
tables (SELECT _ t _ _) = [t]
tables (DESC t _)       = [t]
tables (UPDATE _ t _ _) = [t]
tables (INSERT _ t _)   = [t]
tables (DELETE t _ _)   = [t]

-- | Test whether or not a query contains the ME keyword in the where
-- clause
usingMe :: Statement -> Bool
usingMe stmt = case stmt
               of (SELECT _ _ w _) -> Just True == fmap findMe w
                  (DESC _ _)       -> False
                  (UPDATE c _ w _) -> Just True == fmap findMe w
                                      || any (MeValue==) (map snd c)
                  (INSERT c _ _)   -> any (MeValue==) (map snd c)
                  (DELETE _ w _)   -> Just True == fmap findMe w
  where findMe (_ `OpEq` v)    = v == MeValue
        findMe (_ `OpIn` vs)   = any (==MeValue) vs
        findMe (w0 `OpAnd` w1) = findMe w0 || findMe w1
        findMe (w0 `OpOr` w1)  = findMe w0 || findMe w1

functions :: Statement -> [Function]
functions (SELECT _ _ _ f) = f
functions (DESC _ f)       = f
functions (UPDATE _ _ _ f) = f
functions (INSERT _ _ f)   = f
functions (DELETE _ _ f)   = f

showStmt :: Statement -> String
showStmt stmt = case stmt
                of DESC tbl func             -> "DESC "
                                                ++ tbl
                                                ++ funcString func
                                                ++ ";"
                   SELECT cols tbl whre func -> "SELECT "
                                                ++ intercalate "," cols
                                                ++ " FROM "
                                                ++ tbl
                                                ++ whereString whre
                                                ++ funcString func
                                                ++ ";"
                   UPDATE set tbl whre func  -> "UPDATE "
                                                ++ tbl
                                                ++ " SET "
                                                ++ intercalate "," (map (\(k,v) -> k++"="++showValue v) set)
                                                ++ whereString whre
                                                ++ funcString func
                                                ++ ";"
                   INSERT set tbl func       -> "INSERT INTO "
                                                ++ tbl
                                                ++ " ("
                                                ++ intercalate "," (map fst set)
                                                ++ ") VALUES ("
                                                ++ intercalate "," (map (showValue . snd) set)
                                                ++ ")"
                                                ++ funcString func
                                                ++ ";"
                   DELETE tbl whre func      -> "DELETE FROM "
                                                ++ tbl
                                                ++ whereString whre
                                                ++ funcString func
                                                ++ ";"

  where funcString func | null func = ""
                        | otherwise = " | " ++ intercalate " | " (map show func)
        
        whereString Nothing  = ""
        whereString (Just w) = " WHERE "++ (show w)

readStmt :: String -> Either ParseError Statement
readStmt = flip parseYql builder

readDescXml :: XML -> Maybe Description
readDescXml xml = case (map toLower securityAttr)
                  of "user" -> fmap (\n -> Table n User httpsAttr) (attr "name")
                     "app"  -> fmap (\n -> Table n App httpsAttr) (attr "name")
                     _      -> fmap (\n -> Table n Any httpsAttr) (attr "name")
  where attr k = join (fmap (attribute k) (findElement "table" xml))
        Just securityAttr = attr "security" `mplus` Just "ANY"
        httpsAttr = Just "true" == attr "https"

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
  readsPrec _ input = case (readStmt input)
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

instance Ord Security where
  compare Any Any   = EQ
  compare App App   = EQ
  compare User User = EQ
  compare Any _     = LT
  compare User _    = GT
  compare App Any   = GT
  compare App User  = LT

instance Linker () where
  link _ _ _ = Nothing

instance Linker (String,[(String,Value)] -> Maybe Exec) where
  link (k0,f) k1 argv | k1==k0    = (f argv)
                      | otherwise = Nothing

instance Linker l => Linker [l] where
  link r k argv = foldr mplus Nothing (zipWith ($) (map (uncurry . link) r) (repeat (k,argv)))
