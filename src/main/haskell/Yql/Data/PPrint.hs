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
       , Color(..)
       , Style(..)
       , Device(..)
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
         -- * Style
       , style
         -- * Query
       , width
         -- * Rendering
       , render
       , renderTo
       ) where

import Text.Printf

data Color = Black
           | White
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | None
           deriving (Eq)

data Device = Memory
            | Terminal
            deriving (Eq)

data Style = Style { fgcolor :: Color
                   , bgcolor :: Color
                   , bold    :: Bool
                   }
           | Plain

data Doc = Text (Style,String) Doc
         | Line Int Doc
         | Space Int Doc
         | Nil

-- ctext :: Int -> Char -> Doc
-- ctext k = text . replicate k

-- stext :: Show a => a -> Doc
-- stext = text . show

-- lspace :: Int -> Doc -> Doc
-- lspace by = space by

color :: (Style -> Color) -> Style -> Color
color _ Plain = None
color f s = f s

-- | Applies a given style to the document
style :: Style -> Doc -> Doc
style s0 (Text (s1,t) d) = Text (s0,t) (style s1 d)
style s (Line n d)       = Line n (style s d)
style s (Space n d)      = Space n (style s d)
style _ Nil              = Nil

-- | Add space to the right
rspace :: Int -> Doc -> Doc
rspace by = (+++ (space by Nil))

text :: String -> Doc
text t = Text (Plain,t) Nil

newline :: Doc -> Doc
newline = Line 0

space :: Int -> Doc -> Doc
space m (Space n d) = Space (m+n) d
space m d           = Space m d

-- | Left-padding all newlines in the following document
nest :: Int -> Doc -> Doc
nest m (Text t d)  = Text t (nest m d)
nest m (Line n d)  = Line (m+n) (nest m d)
nest m (Space n d) = Space n (nest m d)
nest _ Nil         = Nil

-- | Same as nest but uses a given document as the padding
nestWith :: Doc -> Doc -> Doc
nestWith m (Text t d)  = Text t (nestWith m d)
nestWith m (Line n d)  = Line n (m +++ nestWith m d)
nestWith m (Space n d) = Space n (nestWith m d)
nestWith _ Nil         = Nil

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
-- columns (Text t d)  = length s + (columns d)
-- columns (Space m d) = m + (columns d)
-- columns _           = 0

(+++) :: Doc -> Doc -> Doc
(Text t d) +++ x  = Text t (d +++ x)
(Line n d) +++ x  = Line n (d +++ x)
(Space n d) +++ x = Space n (d +++ x)
Nil +++ x         = x
infixr 9 +++

renderTo :: Device -> Doc -> String
renderTo _ Nil           = ""
renderTo dev (Space k d) = (replicate k ' ') ++ renderTo dev d
renderTo dev (Line k d)  = "\n" ++ (replicate k ' ') ++ renderTo dev d
renderTo dev (Text t d)  = dump t ++ renderTo dev d
  where dump (s,v)
          | dev == Terminal = printf "\x1b[%dm\x1b[%dm\x1b[%dm%s\x1b[0m" (termBold :: Int) termFg termBg v
          | otherwise       = v
            where termFg   = color2ansi (30+) 39 (color fgcolor s)
                  termBg   = color2ansi (40+) 49 (color bgcolor s)
                  termBold = if (bold s)
                             then 1
                             else 22

render :: Doc -> String
render = renderTo Memory

color2ansi :: (Int -> Int) -> Int -> Color -> Int
color2ansi f _ Black   = f 0
color2ansi f _ Red     = f 1
color2ansi f _ Green   = f 2
color2ansi f _ Yellow  = f 3
color2ansi f _ Blue    = f 4
color2ansi f _ Magenta = f 5
color2ansi f _ Cyan    = f 6
color2ansi f _ White   = f 7
color2ansi _ def _     = def

instance Show Doc where
  showsPrec _ = showString . render
