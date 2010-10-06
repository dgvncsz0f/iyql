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

module Yql.Data.PPrint
       ( -- * Types 
         Doc()
         -- * Combinators
       , text
       , newline
       , space
       , nest
       , nestWith
       , empty
       , cat
       , rspace
       , (+++)
         -- * Query
       , width
         -- * Rendering
       , render
       ) where

data Doc = Text String Doc
         | Line Int Doc
         | Space Int Doc
         | Nil

-- ctext :: Int -> Char -> Doc
-- ctext k = text . replicate k

-- stext :: Show a => a -> Doc
-- stext = text . show

-- lspace :: Int -> Doc -> Doc
-- lspace by = space by

-- | Add space to the right
rspace :: Int -> Doc -> Doc
rspace by = (+++ (space by Nil))

text :: String -> Doc
text = flip Text Nil

newline :: Doc -> Doc
newline = Line 0

space :: Int -> Doc -> Doc
space m (Space n d) = Space (m+n) d
space m d           = Space m d

-- | Left-padding all newlines in the following document
nest :: Int -> Doc -> Doc
nest _ Nil         = Nil
nest m (Text s d)  = Text s (nest m d)
nest m (Line n d)  = Line (m+n) (nest m d)
nest m (Space n d) = Space n (nest m d)

-- | Same as nest but uses a given document as the padding
nestWith :: Doc -> Doc -> Doc
nestWith _ Nil         = Nil
nestWith m (Text s d)  = Text s (nestWith m d)
nestWith m (Line n d)  = Line n (m +++ nestWith m d)
nestWith m (Space n d) = Space n (nestWith m d)

-- | The empty document, which in general is rendered as the null string.
empty :: Doc
empty = Nil

-- | Queries all the lines and returns the maximum line.
width :: Doc -> Int
width = maximum . (0:) . map length . lines . show

-- | Concatenates a list of documents with newlines.
cat :: [Doc] -> Doc
cat = foldr ((+++) . newline) empty 

-- columns :: Doc -> Int
-- columns (Text s d)  = length s + (columns d)
-- columns (Space m d) = m + (columns d)
-- columns _           = 0

(+++) :: Doc -> Doc -> Doc
(Text s d) +++ x  = Text s (d +++ x)
(Line n d) +++ x  = Line n (d +++ x)
(Space n d) +++ x = Space n (d +++ x)
Nil +++ x         = x
infixr 9 +++

render :: Doc -> String
render Nil         = ""
render (Space k d) = (replicate k ' ') ++ render d
render (Line k d)  = "\n" ++ (replicate k ' ') ++ render d
render (Text s d)  = s ++ render d

instance Show Doc where
  showsPrec _ = showString . render
