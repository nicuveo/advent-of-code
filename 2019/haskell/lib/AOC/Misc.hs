-- module

module AOC.Misc where



-- the blackbird

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)
infixr 8 ...



-- other useful combinators

(<.) :: (b -> a -> c) -> (a -> b) -> a -> c
(<.) = (<*>) . flip

(.>) :: (a -> b -> c) -> (a -> b) -> a -> c
(.>) = (<*>)
