-- module

module Day25 (day25_1, day25_2) where



-- import

import           Data.Function
import qualified Data.Map.Strict as M

import           Common



-- solution

day25_1 :: Solution
day25_1 _ = show $ countIf toEnum $ M.elems $ steps 12425180 initialState
  where steps 0 = tape
        steps n = steps (n-1) . step


day25_2 :: Solution
day25_2 = const "Merry Christmas, happy holidays!"



-- types

data Label = A | B | C | D | E | F
  deriving (Show, Eq)

type Tape  = M.Map Int Int

data State = State { label :: Label
                   , tape  :: Tape
                   , pos   :: Int
                   }



-- transitions

initialState :: State
initialState = State A M.empty 0

get :: State -> Int
get s = M.findWithDefault 0 (pos s) $ tape s

set :: Int -> State -> State
set i s = s { tape = M.insert (pos s) i $ tape s }

moveR :: State -> State
moveR s = s { pos = pos s + 1 }

moveL :: State -> State
moveL s = s { pos = pos s - 1 }

to :: Label -> State -> State
to l s = s { label = l }

step :: State -> State
step s = s & case (label s, get s) of
               (A, 0) -> to B . moveR . set 1
               (A, 1) -> to F . moveR . set 0
               (B, 0) -> to B . moveL . set 0
               (B, 1) -> to C . moveL . set 1
               (C, 0) -> to D . moveL . set 1
               (C, 1) -> to C . moveR . set 0
               (D, 0) -> to E . moveL . set 1
               (D, 1) -> to A . moveR . set 1
               (E, 0) -> to F . moveL . set 1
               (E, 1) -> to D . moveL . set 0
               (F, 0) -> to A . moveR . set 1
               (F, 1) -> to E . moveL . set 0
               (_, _) -> error "day25: unexpected value on tape"
