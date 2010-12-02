{-# LANGUAGE
        MultiParamTypeClasses,
        TemplateHaskell,
        TypeSynonymInstances,
        UndecidableInstances
  #-}

module TreeTrie where

import Prelude hiding (lookup)

import Data.Derive.Trie
import Data.KeyMap

data Tree a = Leaf a | Node (Tree a) (Tree a)

type IntTree = Tree Int

deriveTrie [''IntTree]

t1 = insert (Leaf 3) "first" (empty :: IntTreeTrie String)
t2 = insert (Node (Leaf 1) (Leaf 2)) "second" t1

main = do
    print $ lookup (Leaf 1) t2
    print $ lookup (Leaf 3) t2
    print $ lookup (Node (Leaf 2) (Leaf 1)) t2
    print $ lookup (Node (Leaf 1) (Leaf 2)) t2

