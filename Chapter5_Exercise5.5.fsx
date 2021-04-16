(*
    Exercise 5.5
    Code based on Section 4.6, Example: Map colouring
*)
type Country = string
type Map = (Country * Country) list

type Colour = Country list
type Colouring = Colour list

let exMap = [("a","b"); ("c","d"); ("d","a")]

let rec isMember x = function
    | y::ys -> x=y || (isMember x ys)
    | [] -> false

let areNb m c1 c2 =
    List.exists (fun x -> x = (c1,c2) || x = (c2,c1)) m

areNb exMap "a" "d" // true
areNb exMap "a" "c" // false

let rec canBeExtBy m col c =
    List.forall (fun c' -> not(areNb m c' c)) col

canBeExtBy exMap ["c"] "a"
canBeExtBy exMap ["a"; "c"] "b"
canBeExtBy exMap ["a"] "b"

let extColouring m cols c =
    let cols' = List.fold (fun e colour -> if canBeExtBy m colour c
                                           then e @ [(c::colour)]
                                           else e @ [colour] )
                          [] cols
    in if cols' = cols 
       then [c]::cols 
       else cols'

extColouring exMap [] "a"
extColouring exMap [["c"]] "a"
extColouring exMap [["b"]] "a"

let addElem x ys = if isMember x ys then ys else x::ys

let countries m = 
    List.foldBack (fun x e -> let (x1,x2) = x in addElem x1 (addElem x2 e)) m []

countries exMap

// Order of processing country list matters!
// You can compare with commented variant based on List.fold
//let colCntrs m cs = List.fold (fun e x -> extColouring m e x) [] cs
let colCntrs m cs = List.foldBack (fun x e -> extColouring m e x) cs []

let colMap m = colCntrs m (countries m)

colMap exMap
