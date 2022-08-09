module SymGrp exposing (..)

type alias Permutation = List Int

degree : Permutation -> Int
degree s = List.length s

act : Permutation -> Int -> Int
act p i =
    if i > (degree p) then
        i
    else
        Maybe.withDefault 0 <| List.head <|List.drop (i-1) p

prod : Permutation -> Permutation -> Permutation
prod p q =
    List.map (\i -> act p i) q

inv : Permutation -> Permutation
inv p =
    let
        f = List.indexedMap (\i s -> {src=(i+1), dst=s}) p
    in
        List.map .src <| List.sortBy .dst f

        
pow : Permutation -> Int -> Permutation
pow sigma k =
    let
        a = if k < 0 then
                -k
            else
                k
        p = if k < 0 then
                inv sigma
            else
                sigma
    in
        case a of
            0 -> List.range 1 (List.length p)
            1 -> p
            _ -> prod p (pow p (a-1))

cycle : List Int -> Int -> Permutation
cycle list deg =
    let
        src = list
        dst = List.concat [(List.drop 1 list)
                          ,(List.take 1 list)
                          ]
        map = List.map2 (\s d -> {src=s, dst=d}) src dst
        completed = List.foldl (\s partial ->
                                    if List.member s (List.map .src map) then
                                        partial
                                    else
                                        partial ++ [{src=s, dst=s}]
                               ) map (List.range 1 deg)
    in
        List.map .dst <| List.sortBy .src completed
