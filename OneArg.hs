{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Generic.Diff

data OneArg = None | One Int

-- The second step is creating the family datatype. Each constructor
-- in the datatypes above gets a constructor in a family GADT:

data OneArgFamily :: * -> * -> * where
    None' ::          OneArgFamily OneArg Nil
    One'  ::          OneArgFamily OneArg (Cons Int Nil)
    Int'  ::  Int ->  OneArgFamily Int  Nil              


instance Family OneArgFamily where
    decEq None'   None'    =                Just (Refl, Refl)
    decEq One'  One'       =                Just (Refl, Refl)
    decEq (Int' i)  (Int' j) | i == j     = Just (Refl, Refl)
                             | otherwise  = Just (Refl, Refl)
    decEq _     _      = Nothing

    fields None'  None     = Just CNil
    fields One'  (One i)   = Just (CCons i CNil)
    fields (Int' _)  _     = Just CNil
    fields _         _     = Nothing

    apply None'      CNil           = None
    apply One'       (CCons i CNil) = One i
    apply (Int' i)   CNil           = i

    string None'    = "None"
    string One'   = "One"
    string (Int' i)   = show i

instance Type OneArgFamily OneArg where
  constructors = [ Concr None'
                 , Concr One'
                 ]

instance Type OneArgFamily Int where
  constructors = [ Abstr Int' ]

test :: EditScript OneArgFamily OneArg OneArg
test = diff None (One 1)
