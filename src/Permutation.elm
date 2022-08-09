module Permutation exposing (..)

type alias Permutation = List Int

degree : Permutation -> Int
degree s = List.length s

act : Permutation -> Int -> Maybe Int
act p i =
    List.head <|List.drop (i-1) p

--prod : Permutation -> Permutation -> Permutation
--prod 
