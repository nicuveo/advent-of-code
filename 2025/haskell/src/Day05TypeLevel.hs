{-# LANGUAGE UndecidableInstances #-}

module Day05TypeLevel where

import "this" Prelude


data Zero
data Succ n

type N0   = Zero
type N1   = Succ N0
type N2   = Succ N1
type N3   = Succ N2
type N4   = Succ N3
type N5   = Succ N4
type N6   = Succ N5
type N7   = Succ N6
type N8   = Succ N7
type N9   = Succ N8
type N10  = Succ N9
type N11  = Succ N10
type N12  = Succ N11
type N13  = Succ N12
type N14  = Succ N13
type N15  = Succ N14
type N16  = Succ N15
type N17  = Succ N16
type N18  = Succ N17
type N19  = Succ N18
type N20  = Succ N19
type N21  = Succ N20
type N22  = Succ N21
type N23  = Succ N22
type N24  = Succ N23
type N25  = Succ N24
type N26  = Succ N25
type N27  = Succ N26
type N28  = Succ N27
type N29  = Succ N28
type N30  = Succ N29
type N31  = Succ N30
type N32  = Succ N31

type family Add x y where
  Add Zero y = y
  Add x Zero = x
  Add x (Succ y) = Add (Succ x) y


data Range b e

data True
data False


type family GE x y where
  GE x Zero = True
  GE Zero y = False
  GE (Succ x) (Succ y) = GE x y

type family LE x y where
  LE Zero y = True
  LE x Zero = False
  LE (Succ x) (Succ y) = LE x y


type family And x y where
  And True True = True
  And b1   b2   = False


type family BoolToNum b where
  BoolToNum False = N0
  BoolToNum True  = N1

type family Within x r where
  Within x (Range b e) = And (GE x b) (LE x e)

type family WithinAny x rs where
  WithinAny x Nil         = False
  WithinAny x (Cons r rs) = WithinAny' (Within x r) x rs

type family WithinAny' b x rs where
  WithinAny' True  x rs = True
  WithinAny' False x rs = WithinAny x rs

data Nil
data Cons x l


type family SetMember x s where
  SetMember x Nil        = False
  SetMember x (Cons x s) = True
  SetMember x (Cons i s) = SetMember x s

type family SetInsert x s where
  SetInsert x Nil        = Cons x Nil
  SetInsert x (Cons x s) = Cons x s
  SetInsert x (Cons i s) = Cons i (SetInsert x s)

type family SetInsertAll xs s where
  SetInsertAll Nil         s = s
  SetInsertAll (Cons x xs) s = SetInsertAll xs (SetInsert x s)

type family SetCount s where
  SetCount Nil        = Zero
  SetCount (Cons i s) = Succ (SetCount s)

type family Enumerate r where
  Enumerate (Range b b) = Cons b Nil
  Enumerate (Range b e) = Cons b (Enumerate (Range (Succ b) e))



type family Part1 rs is where
  Part1 rs is = Part1' N0 rs is

type family Part1' accum rs is where
  Part1' accum rs Nil         = accum
  Part1' accum rs (Cons i is) =
    Part1' (Add accum (BoolToNum (WithinAny i rs))) rs is


type family Part2 rs where
  Part2 rs = Part2' Nil rs

type family Part2' s rs where
  Part2' s Nil         = SetCount s
  Part2' s (Cons r rs) = Part2' (SetInsertAll (Enumerate r) s) rs


class Reify a where
  reify :: Int

instance Reify Zero where
  reify = 0

instance Reify n => Reify (Succ n) where
  reify = succ $ reify @n

type ExampleRanges =
  ( Cons (Range  N3  N5)
  ( Cons (Range N10 N14)
  ( Cons (Range N16 N20)
  ( Cons (Range N12 N18)
  ( Nil )))))

type ExampleIngredients =
  ( Cons N1
  ( Cons N5
  ( Cons N8
  ( Cons N11
  ( Cons N17
  ( Cons N32
  ( Nil )))))))

run :: IO ()
run = do
  print $ reify @(Part1 ExampleRanges ExampleIngredients)
  print $ reify @(Part2 ExampleRanges)
