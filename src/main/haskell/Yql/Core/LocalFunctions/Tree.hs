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

module Yql.Core.LocalFunctions.Tree
       ( function
       ) where

import Yql.Data.PPrint
import Yql.Data.Xml
import Yql.Core.LocalFunction
import Data.Char

data Tree = Branch String [Tree]
          | Leaf String String

function :: Exec
function = Transform (const doc) (const $ render . xml2doc)
  where doc = unlines [ "Reads the xml output and transforms it into a tree-like format"
                      ]

showTree :: Tree -> Doc
showTree (Branch k []) = text "+- " <> text k
showTree (Branch k xs) = text "+- " <> text k <> nestWith (text "|  ") (newline (cat $ map showTree xs))
showTree (Leaf k v)    = text "+- " <> text k <> text ": " <> text v

xml2doc :: String -> Doc
xml2doc raw = text "results" <> newline (cat $ (map (showTree . xml2tree) nodes))
  where Just xml   = xmlParse raw
        Just nodes = fmap (filter element . childNodes) (findElement "results" xml)

xml2tree :: XML -> Tree
xml2tree xml = Branch label subtree
  where label      
          | hasContent = tagName xml ++ ": " ++ content
          | otherwise  = tagName xml
        
        content = concatMap verbatim (filter pcdata (childNodes xml))
        
        subtree = map xml2tree (filter element (childNodes xml)) ++ map (\(k,v) -> Leaf ('@':k) v) (attributes xml)

        hasContent = not (null (filter (not . isSpace) content))
