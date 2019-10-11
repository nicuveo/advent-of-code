-- module

module AOC.Misc where



-- the blackbird

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)
infixr 8 ...



-- other useful combinators

(<.) :: (a -> b) -> (b -> a -> c) -> a -> c
(<.) = (>>=)

(.>) :: (a -> b -> c) -> (a -> b) -> a -> c
(.>) = (<*>)
