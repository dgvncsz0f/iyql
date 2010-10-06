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

module Yql.Core.LocalFunctions.Tables
       ( function
       ) where

import Yql.Data.PPrint
import Yql.Data.Xml
import Yql.Core.LocalFunction
import Data.List
import qualified Data.Map as M

newtype Column = Column { unColumn :: (String,[Cell]) }
               deriving (Show)

data Table = Lines { rows :: [Column] }
               deriving (Show)

data Cell = Complex Table
          | Scalar String
               deriving (Show)

function :: Exec
function = Transform (const doc) (const $ render . xml2doc)
  where doc = unlines [ "Reads the xml output and transform it into tabular form."
                      ]

norm :: Table -> Table
norm (Lines cols) = Lines (map fixColumn cols)
  where maxHeight = maximum (map (length . snd . unColumn) cols)

        fixHeight xs = (map dig xs) ++ replicate (maxHeight - (length xs)) (Scalar "")
          where dig (Complex t) = Complex (norm t)
                dig scalar      = scalar

        fixColumn (Column (h,cs)) = Column (h,fixHeight cs)

showCell :: Cell -> (Int,Doc)
showCell (Scalar s)  = (length s,text . unwords . lines $ s)
showCell (Complex t) = let doc = showTable t
                       in (width doc,doc)

maxWidth :: Column -> Int
maxWidth (Column (h,cs)) = maximum (hSize : map fst cellStr)
  where cellStr = map showCell cs
        hSize   = length h

showColumn :: Column -> [(Int,Doc)]
showColumn (Column (h,cs)) = (myMaxWidth,text header) : map (\(_,b) -> (myMaxWidth,b)) (map showCell cs)
  where header     = "*" ++ h ++ "*"
        myColumn   = Column (header,cs)
        myMaxWidth = maxWidth myColumn

showTable :: Table -> Doc
showTable = cat . map (showTable_ 1) . transpose . map showColumn . rows
  where showTable_ _ []           = text "|"
        showTable_ acc ((w,x):xs) = nest acc (text "|"
                                              <> rspace (w - width x) x)
                                  <> showTable_ (acc+w+1) xs

xml2doc :: String -> Doc
xml2doc xml = showTable . xml2table $ results
 where Just doc     = xmlParse xml
       Just results = fmap (childNodes) (findElement "results" doc)

xml2table :: [XML] -> Table
xml2table = unpack . build . xmlRows
  where xmlRows tag = map (map xmlCols . filter element . childNodes) tag

        xmlCols tag | simple    = (tagName tag,[Scalar (verbatim tag)])
                    | otherwise = (tagName tag,[Complex (xml2table [tag])])
          where simple = all pcdata (childNodes tag)

        build = foldr (M.unionWith (++)) M.empty . map (M.fromListWith (++))

        unpack = norm . Lines . map Column . M.toList
