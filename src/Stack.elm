module Stack exposing (..)

type alias Stack a = List a

empty: Stack a
empty = []

push: Stack a -> a -> Stack a
push stack a = a :: stack

pop: Stack a -> Maybe a
pop stack = case stack of
    head :: _ -> Just head
    _ -> Nothing