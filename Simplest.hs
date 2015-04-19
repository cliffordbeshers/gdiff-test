{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Generic.Diff


data Simplest = No | Yes

-- The second step is creating the family datatype. Each constructor
-- in the datatypes above gets a constructor in a family GADT:

data SimplestFamily :: * -> * -> * where
    No'     ::          SimplestFamily Simplest Nil
    Yes'    ::          SimplestFamily Simplest Nil


instance Family SimplestFamily where
    decEq No'   No'    =              Just (Refl, Refl)
    decEq Yes'  Yes'   =              Just (Refl, Refl)
    decEq _     _      = Nothing

    fields No'  No    = Just CNil
    fields Yes' Yes   = Just CNil
    fields _         _           = Nothing

    apply No'      CNil  = No
    apply Yes'     CNil = Yes

    string No'    = "No"
    string Yes'   = "Yes"

instance Type SimplestFamily Simplest where
  constructors = [Concr No', Concr Yes']

test :: EditScript SimplestFamily Simplest Simplest
test = diff No Yes
