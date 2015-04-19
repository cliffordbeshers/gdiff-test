{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Generic.Diff


data Expr  =  Min Expr Term
data Term  =  Parens Expr
           |  Number Int

-- The second step is creating the family datatype. Each constructor
-- in the datatypes above gets a constructor in a family GADT:

data ExprTermFamily :: * -> * -> * where
    Min'     ::          ExprTermFamily Expr (Cons Expr (Cons Term Nil))
    Parens'  ::          ExprTermFamily Term (Cons Expr Nil)
    Number'  ::          ExprTermFamily Term (Cons Int Nil)
    Int'     ::  Int ->  ExprTermFamily Int  Nil              


instance Family ExprTermFamily where
    decEq Min'      Min'      =              Just (Refl, Refl)
    decEq Parens'   Parens'   =              Just (Refl, Refl)
    decEq Number'   Number'   =              Just (Refl, Refl)
    decEq (Int' x)  (Int' y)  | x == y     = Just (Refl, Refl)
                              | otherwise  = Nothing
    decEq _        _          = Nothing

    fields Min'      (Min e t)   = Just (CCons e (CCons t CNil))
    fields Parens'   (Parens e)  = Just (CCons e CNil)
    fields Number'   (Number i)  = Just (CCons i CNil)
    fields (Int' _)  _           = Just CNil
    fields _         _           = Nothing

    apply Min'      (CCons e (CCons t CNil))  = Min e t
    apply Parens'   (CCons e CNil)            = Parens e
    apply Number'   (CCons i CNil)            = Number i
    apply (Int' i)  CNil                      = i

    string Min'      = "Min"
    string Parens'   = "Parens"
    string Number'   = "Number"
    string (Int' i)  = show i

instance Type ExprTermFamily Term where
  constructors = [Concr Number', Concr Parens']

instance Type ExprTermFamily Expr where
  constructors = [Concr Min']

instance Type ExprTermFamily Int where
  constructors = [Abstr Int']
