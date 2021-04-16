(*
    Section 4.6
    Example: Map colouring
*)
type Country = string
type Map = (Country * Country) list

type Colour = Country list
type Colouring = Colour list

let rec isMember x = function
    | y::ys -> x = y || (isMember x ys)
    | [] -> false

let addElem x ys = if isMember x ys then ys else x::ys
let rec countries = function
    | [] -> []
    | (c1,c2)::m -> addElem c1 (addElem c2 (countries m))

let colMap m = 
    let areNb c1 c2 =
        isMember (c1,c2) m || isMember (c2,c1) m
    let rec canBeExtBy col c =
        match col with
            | [] -> true
            | c'::col' -> not(areNb c' c) && canBeExtBy col' c
    let rec extColouring cols c =
        match cols with
            | [] -> [[c]]
            | col::cols' -> if canBeExtBy col c
                            then (c::col)::cols'
                            else col::extColouring cols' c
    let rec colCntrs = function
        | [] -> []
        | c::cs -> extColouring (colCntrs cs) c
    colCntrs (countries m)

let exMap = [("a","b"); ("c","d"); ("d","a")]
colMap exMap

(*
    Exercise 4.20
    In set of auxiliary function "m" argument is used in "areNb" and "countries" only.
*)