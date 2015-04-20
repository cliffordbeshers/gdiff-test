{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TreeDiff where

import Data.Generic.Diff


data A = A
data Tree = Node A (Forest)

data Forest = Forest Tree Forest | ForestNil


-- The second step is creating the family datatype. Each constructor
-- in the datatypes above gets a constructor in a family GADT:

data ForestTreeFamily :: * -> * -> * where
    Node' ::          ForestTreeFamily Tree (Cons A (Cons Forest Nil))
    Forest'  ::          ForestTreeFamily Forest (Cons Tree (Cons Forest Nil))
    ForestNil'  ::    ForestTreeFamily Forest Nil
    A'  ::    ForestTreeFamily A Nil


instance Family ForestTreeFamily where
    decEq Node'   Node'    =                Just (Refl, Refl)
    decEq Forest'  Forest'       =          Just (Refl, Refl)
    decEq ForestNil'  ForestNil' =          Just (Refl, Refl)
    decEq A'  A'            =          Just (Refl, Refl)
    decEq _     _      = Nothing

    fields Node'  (Node a t)    = Just (CCons a (CCons t CNil))
    fields Forest'  (Forest a f)   = Just (CCons a (CCons f CNil))
    fields ForestNil'  ForestNil  = Just CNil
    fields A'  A  = Just CNil
    fields _         _     = Nothing

    apply Node'      (CCons a (CCons t CNil))  = Node a t
    apply Forest'   (CCons a (CCons f CNil))     = Forest a f
    apply ForestNil'  CNil = ForestNil
    apply A'  CNil = A

    string Node'    = "Node"
    string Forest'   = "Forest"
    string ForestNil' = "ForestNil"
    string A'  = "A"

instance Type ForestTreeFamily Tree where
  constructors = [ Concr Node'
                 ]

instance Type ForestTreeFamily Forest where
  constructors = [ Concr Forest', Concr ForestNil' ]

instance Type ForestTreeFamily A where
  constructors = [ Concr A' ]

test :: EditScript ForestTreeFamily Forest Forest
test = diff (ForestNil) (Forest (Node A ForestNil) ForestNil)

test2 :: EditScript ForestTreeFamily Forest Forest
test2 = diff (Forest (Node A ForestNil) ForestNil) (Forest (Node A ForestNil) ForestNil)
