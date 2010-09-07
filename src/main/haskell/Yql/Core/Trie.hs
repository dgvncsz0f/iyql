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
       , Yql.Core.Trie.null 
       , member
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

import Data.Maybe (isJust)
import qualified Data.Map as M
import Debug.Trace

data Trie k = Trie (M.Map k (Bool,Trie k))
            deriving (Show,Eq,Ord)

fold :: ([k] -> a -> a) -> a -> Trie k -> a
fold f0 z0 = fst . fold' f0 (z0,id)
  where fold' f z (Trie m) = M.foldrWithKey g z m
          where g k (leaf,t) (acc,ks)
                  | leaf      = (f (ks.(k:) $ []) (fold h acc t), ks)
                  | otherwise = fold' h (acc, ks) t
                    where h = f . (k:)

-- | Test if a given Trie is empty
null :: Trie k -> Bool
null (Trie m) = M.null m

-- | Creates an empty trie
empty :: Trie k
empty = Trie M.empty

-- | Returns a trie with a single value
singleton :: Ord k => [k] -> Trie k
singleton ks = fromList [ks]

-- | Maps a trie into list type, such as `toList . fromList = id'
toList :: Show k => Trie k -> [[k]]
toList = fold (:) []

-- | Maps a list into trie, such as `fromList . toList = id'.
fromList :: Ord k => [[k]] -> Trie k
fromList = foldr union empty . map fromList'
  where fromList' (k:ks) = Trie (M.singleton k (Prelude.null ks,fromList' ks))
        fromList' []     = empty

-- | The number of entries in the trie.
size :: Show k => Trie k -> Int
size = fold (const (1+)) 0

union :: Ord k => Trie k -> Trie k -> Trie k
union (Trie v0) (Trie v1) = Trie (M.unionWith unionValue v0 v1)
  where unionValue (a,t0) (b,t1) = (a || b,t0 `union` t1)

-- | Performs a prefix search.
subtrie :: Ord k => [k] -> Trie k -> Trie k
subtrie = flip (foldl f)
  where f (Trie m) k = snd $ M.findWithDefault (False,empty) k m

-- | Test whether a given prefix is in the trie.
member :: Ord k => [k] -> Trie k -> Bool
member ks t = fst $ foldl f (True,t) ks
  where f (_,Trie m) k = M.findWithDefault (False,empty) k m
