(*
    Section 4.6
    Example: Map colouring
*)
type Country = string
type Map = (Country * Country) list

type Colour = Country list
type Colouring = Colour list

let rec isMember x = function
    | y::ys -> x=y || (isMember x ys)
    | [] -> false;;

let areNb m c1 c2 =
    isMember (c1,c2) m || isMember (c2,c1) m

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

let addElem x ys = if isMember x ys then ys else x::ys
let rec countries = function
    | [] -> []
    | (c1,c2)::m when c2 = "" -> addElem c1 (countries m)
    | (c1,c2)::m -> addElem c1 (addElem c2 (countries m))
let rec colCntrs m = function
    | [] -> []
    | c::cs -> extColouring m (colCntrs m cs) c

let rec splitMap = function
    | [] -> ([],[])
    | ((a,b) as elem)::mapRest when b = "" -> let (rMainlandMap,rIslandMap) = splitMap mapRest
                                              (rMainlandMap,elem::rIslandMap)
    | elem::mapRest -> let (rMainlandMap,rIslandMap) = splitMap mapRest
                       (elem::rMainlandMap,rIslandMap)

let colMap m = 
    let (mainlandMap,islandMap) = splitMap m in
    let islandColour = countries islandMap in
    let mainlandColouring = colCntrs mainlandMap (countries mainlandMap) in
    match islandColour with
    | [] -> mainlandColouring
    | _ -> islandColour::mainlandColouring

let exMap = [("a","b"); ("c","d"); ("d","a")]
colMap exMap

(*
    Exercise 4.22
    If not to modify type of Map list's element (Country * Country) an island can be represented by 
    pair ("CountryIsland",""). And in a colouring list islands will be in separate element.
    As an example for the input 
    [("d",""),("a","b"),("b","c"),("e","")]
    the ouput is going to be
    [["a","c"],["b"],["d","e"]] or [["d","e"],["a","c"],["b"]]
*)
let exMap2 = [("d",""); ("a","b"); ("b","c"); ("e","")]
colMap exMap2
