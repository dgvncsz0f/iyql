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

-- | A [inefficient] implementation of Trie
module Yql.Core.Trie 
       ( -- * Types
         Trie()
         -- * Query
       , null 
       , size
       , subtrie
         -- * Conversion
       , fromList
       , toList
         -- * Traversal
       , fold
         -- * Construction
       , empty
       , singleton
         -- * Combine
       , union
       ) where

import Prelude hiding (null)
import Data.Maybe (isJust)
import qualified Data.Map as M

data Trie k = Trie (M.Map k (Trie k))
            deriving (Show,Eq,Ord)

fold :: (k -> a, [[a]] -> b) -> Trie k -> b
fold (f,g) (Trie m) = g (M.foldWithKey myFold [] m)
  where myFold k st acc 
          | null st   = [f k] : acc 
          | otherwise = map (f k:) (fold (f,id) st) ++ acc
        
null :: Trie k -> Bool
null (Trie m) = M.null m

empty :: Trie k
empty = Trie M.empty

singleton :: k -> Trie k
singleton k = Trie (M.singleton k empty)

toList :: Trie k -> [[k]]
toList = fold (id,id)

fromList :: Ord k => [[k]] -> Trie k
fromList = foldr union empty . map fromList'
  where fromList' (k:ks) = Trie (M.singleton k (fromList' ks))
        fromList' []     = empty

size :: Trie k -> Int
size = fold (const 1,foldr ((+) . sum) 0)

union :: Ord k => Trie k -> Trie k -> Trie k
union (Trie m0) (Trie m1) = Trie (M.unionWith union m0 m1)

subtrie :: Ord k => [k] -> Trie k -> Trie k
subtrie [] t            = t
subtrie (k:ks) (Trie m) = subtrie ks (M.findWithDefault empty k m)
