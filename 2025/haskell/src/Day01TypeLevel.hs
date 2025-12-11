{-# LANGUAGE UndecidableInstances #-}

module Day01TypeLevel where

import "this" Prelude


-- numbers

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

type family Add x y where
  Add Zero y = y
  Add x Zero = x
  Add x (Succ y) = Add (Succ x) y


-- rotation

data Result position zeroes

type family TurnRight pos delta where
  TurnRight pos delta = TurnRight' pos N0 delta

type family TurnRight' pos zeroes delta where
  TurnRight' pos zeroes Zero     = Result pos zeroes
  TurnRight' N99 zeroes (Succ d) = TurnRight' N0 (Succ zeroes) d
  TurnRight' pos zeroes (Succ d) = TurnRight' (Succ pos) zeroes d

type family TurnLeft pos delta where
  TurnLeft pos delta = TurnLeft' pos N0 delta

type family TurnLeft' pos zeroes delta where
  TurnLeft' pos      zeroes Zero     = Result pos zeroes
  TurnLeft' N0       zeroes (Succ d) = TurnLeft' N99 zeroes d
  TurnLeft' N1       zeroes (Succ d) = TurnLeft' N0 (Succ zeroes) d
  TurnLeft' (Succ p) zeroes (Succ d) = TurnLeft' p zeroes d

data Left  n
data Right n

type family Apply pos step where
  Apply pos (Left  n) = TurnLeft  pos n
  Apply pos (Right n) = TurnRight pos n

type family Position result where
  Position (Result position zeroes) = position


-- list

data Nil
data Cons s i


-- part 1

type family Part1 i where
  Part1 i = Fold1 N50 N0 i

type family Fold1 pos counter instructions where
  Fold1 pos counter Nil =
    counter
  Fold1 pos counter (Cons step instructions) =
    Fold1' (Position (Apply pos step)) counter instructions

type family Fold1' pos counter instructions where
  Fold1' N0  counter instructions = Fold1 N0 (Succ counter) instructions
  Fold1' pos counter instructions = Fold1 pos counter instructions


-- part 2

type family Part2 i where
  Part2 i = Fold2 N50 N0 i

type family Fold2 pos counter instructions where
  Fold2 pos counter Nil =
    counter
  Fold2 pos counter (Cons step instructions) =
    Fold2' (Apply pos step) counter instructions

type family Fold2' result counter instructions where
  Fold2' (Result pos zeroes) counter instructions = Fold2 pos (Add counter zeroes) instructions


-- reify

class Reify a where
  reify :: Int

instance Reify Zero where
  reify = 0

instance Reify n => Reify (Succ n) where
  reify = succ $ reify @n


-- run

type Example =
  ( Cons (Left  N68)
  ( Cons (Left  N30)
  ( Cons (Right N48)
  ( Cons (Left  N5 )
  ( Cons (Right N60)
  ( Cons (Left  N55)
  ( Cons (Left  N1 )
  ( Cons (Left  N99)
  ( Cons (Right N14)
  ( Cons (Left  N82)
  ( Nil )))))))))))

run :: IO ()
run = do
  print $ reify @(Part1 Example)
  print $ reify @(Part2 Example)
