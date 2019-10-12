-- module

module AOC.Misc where



-- enumeration

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]



-- the blackbird

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)
infixr 8 ...



-- other useful combinators

(<.) :: (a -> b) -> (b -> a -> c) -> a -> c
(<.) = (>>=)

(.>) :: (a -> b -> c) -> (a -> b) -> a -> c
(.>) = (<*>)
