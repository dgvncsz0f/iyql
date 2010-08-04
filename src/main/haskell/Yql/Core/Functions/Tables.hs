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

module Yql.Core.Functions.Tables
       ( tables
       ) where

import Yql.Core.Stmt
import Data.List
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Xtract.Parse
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Verbatim
import qualified Data.Map as M
import Text.PrettyPrint

newtype Column = Column { unColumn :: (String,[Cell]) }
               deriving (Show)

data Table = Lines { rows :: [Column] }
               deriving (Show)

data Cell = Complex Table
          | Scalar String
               deriving (Show)

tables :: [(String,Value)] -> Maybe Exec
tables _ = Just (Transform (myRender . xml2table))
  where myRender = renderStyle (Style LeftMode 10000 0)

norm :: Table -> Table
norm (Lines cols) = Lines (map fixColumn cols)
  where maxHeight = maximum (map (length . snd . unColumn) cols)
        
        fixHeight xs = (map mightNorm xs) ++ replicate (maxHeight - (length xs)) (Scalar "")
          where mightNorm (Complex t) = Complex (norm t)
                mightNorm scalar      = scalar
        
        fixColumn (Column (h,cs)) = Column (h,fixHeight cs)

showCell :: Cell -> (Int,Doc)
showCell (Scalar s)  = (length s,text s)
showCell (Complex _) = error "TODO:fixme"

maxWidth :: Column -> Int
maxWidth (Column (h,cs)) = maximum (hSize : map fst cellStr)
  where cellStr = map showCell cs
        hSize   = length h

showColumn :: Column -> [(Int,Doc)]
showColumn c@(Column (h,cs)) = let myMaxWidth = maxWidth c
                               in (myMaxWidth,text h) : map (\(_,b) -> (myMaxWidth,b)) (map showCell cs)

showTable :: Table -> Doc
showTable = vcat . map hSeparate . transpose . map showColumn . rows . norm
  where hSeparate []         = text "|"
        hSeparate ((w,x):xs) = text "|" <+> x $$ nest (w+3) (hSeparate xs)

tagName :: Content i -> String
tagName (CElem (Elem n _ _) _) = n
tagName _                      = error "CElem expected"

childNodes :: Content i -> [Content i]
childNodes (CElem (Elem _ _ cs) _) = cs
childNodes _                       = []

tagContent :: Content i -> [Char]
tagContent = concatMap verbatim . childNodes

textNode :: Content i -> Bool
textNode (CString _ _ _) = True
textNode _               = False

elemNode :: Content i -> Bool
elemNode (CElem _ _) = True
elemNode _           = False

xml2table :: String -> Doc
xml2table xml = showTable $ unpack (build (xmlRows results))
  where Document _ _ docRoot _ = xmlParse "yql xml" xml
        
        results = xtract id "//results/*" (CElem docRoot (posInNewCxt "filename" Nothing))
        
        xmlRows tag = map (map xmlCols . filter elemNode . childNodes) tag
        
        xmlCols tag | simple    = (tagName tag,[Scalar (tagContent tag)])
                    | otherwise = error "TODO:fixme"
          where simple = all textNode (childNodes tag)
        
        build = foldr (M.unionWith (++)) M.empty . map (M.fromListWith (++))
        
        unpack = norm . Lines . map Column . M.toList
