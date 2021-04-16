(*
    Section 4.6
    Example: Map colouring
*)
type Country = string
type Map = (Country * Country) list

type Colour = Country list
type Colouring = Colour list

let isEqual x y = let (y1,y2) = y in let y' = (y2,y1) in x = y || x = y'

let rec isMember1 x = function
    | y::ys -> x = y || (isMember1 x ys)
    | [] -> false

let rec isMember2 x = function
    | y::ys -> isEqual x y || (isMember2 x ys)
    | [] -> false

let areNb m c1 c2 =
    isMember2 (c1,c2) m // || isMember (c2,c1) m

let rec canBeExtBy m col c =
    match col with
        | [] -> true
        | c'::col' -> not(areNb m c' c) && canBeExtBy m col' c

let rec extColouring m cols c =
    match cols with
        | [] -> [[c]]
        | col::cols' -> if canBeExtBy m col c
                        then (c::col)::cols'
                        else col::extColouring m cols' c

let addElem x ys = if isMember1 x ys then ys else x::ys
let rec countries = function
    | [] -> []
    | (c1,c2)::m -> addElem c1 (addElem c2 (countries m))
let rec colCntrs m = function
    | [] -> []
    | c::cs -> extColouring m (colCntrs m cs) c

let colMap m = colCntrs m (countries m)

let exMap = [("a","b"); ("c","d"); ("d","a")]
colMap exMap

(*
    Exercise 4.19
    I tryed to make special comparing at isMember function level. 
    But this solution doesn't work at general.
    It this task polymorth isMember is required. So areNb should be modified.
    As first solution I solved with two variants of isMember.
*)
