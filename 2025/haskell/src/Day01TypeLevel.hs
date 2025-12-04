{-# LANGUAGE UndecidableInstances #-}

module Day01TypeLevel where

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
type N33  = Succ N32
type N34  = Succ N33
type N35  = Succ N34
type N36  = Succ N35
type N37  = Succ N36
type N38  = Succ N37
type N39  = Succ N38
type N40  = Succ N39
type N41  = Succ N40
type N42  = Succ N41
type N43  = Succ N42
type N44  = Succ N43
type N45  = Succ N44
type N46  = Succ N45
type N47  = Succ N46
type N48  = Succ N47
type N49  = Succ N48
type N50  = Succ N49
type N51  = Succ N50
type N52  = Succ N51
type N53  = Succ N52
type N54  = Succ N53
type N55  = Succ N54
type N56  = Succ N55
type N57  = Succ N56
type N58  = Succ N57
type N59  = Succ N58
type N60  = Succ N59
type N61  = Succ N60
type N62  = Succ N61
type N63  = Succ N62
type N64  = Succ N63
type N65  = Succ N64
type N66  = Succ N65
type N67  = Succ N66
type N68  = Succ N67
type N69  = Succ N68
type N70  = Succ N69
type N71  = Succ N70
type N72  = Succ N71
type N73  = Succ N72
type N74  = Succ N73
type N75  = Succ N74
type N76  = Succ N75
type N77  = Succ N76
type N78  = Succ N77
type N79  = Succ N78
type N80  = Succ N79
type N81  = Succ N80
type N82  = Succ N81
type N83  = Succ N82
type N84  = Succ N83
type N85  = Succ N84
type N86  = Succ N85
type N87  = Succ N86
type N88  = Succ N87
type N89  = Succ N88
type N90  = Succ N89
type N91  = Succ N90
type N92  = Succ N91
type N93  = Succ N92
type N94  = Succ N93
type N95  = Succ N94
type N96  = Succ N95
type N97  = Succ N96
type N98  = Succ N97
type N99  = Succ N98
type N100 = Succ N99

data True
data False


type family Add x y where
  Add Zero b = b
  Add a Zero = a
  Add (Succ a) b = Add a (Succ b)

type family Sub x y where
  Sub a Zero = a
  Sub (Succ a) (Succ b) = Sub a b

type family GE x y where
  GE a Zero = True
  GE Zero b = False
  GE (Succ a) (Succ b) = GE a b

type family Mod x y where
  Mod x y = Mod' (GE x y) x y

type family Mod' ge x y where
  Mod' True  x y = Mod (Sub x y) y
  Mod' False x y = x

data Left  n
data Right n

data Nil
data Step s i

type family Part1 i where
  Part1 i = Fold N50 N0 i

type family Fold position counter instructions where
  Fold position counter Nil =
    counter
  Fold position counter (Step (Right n) instructions) =
    Fold' (Mod (Add position n) N100) counter instructions
  Fold position counter (Step (Left n) instructions) =
    Fold' (Mod (Sub (Add position N100) n) N100) counter instructions

type family Fold' position counter instructions where
  Fold' Zero counter instructions = Fold Zero (Succ counter) instructions
  Fold' pos  counter instructions = Fold pos counter instructions


class Reify a where
  reify :: Int

instance Reify Zero where
  reify = 0

instance Reify n => Reify (Succ n) where
  reify = succ $ reify @n


type Example =
  ( Step (Left  N68)
  ( Step (Left  N30)
  ( Step (Right N48)
  ( Step (Left  N5 )
  ( Step (Right N60)
  ( Step (Left  N55)
  ( Step (Left  N1 )
  ( Step (Left  N99)
  ( Step (Right N14)
  ( Step (Left  N82)
  ( Nil )))))))))))

main :: IO ()
main = do
  print $ reify @(Part1 Example)
