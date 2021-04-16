(*
    Exercise 4.1
    Declare function upto: int -> int list such that upto n = [1; 2; . . . ; n].
    Decompose as 1 :: [2;3] or as [1;2]@[3]
*)
// Solution 1
// let rec upto n = 
//     match n with
//         | 1 -> [1]
//         | _ -> upto (n-1) @ [n]
// upto 5

// The solution is based on Section "Simple list expressions"
// let upto n = [1..n]
// upto 5

// The solution is based on the idea from exercise 2.7.1
let rec list_from_to (a,b) =
    if a > b then []
    else a :: list_from_to (a+1,b)
let upto n = list_from_to (1,n)
upto 5
upto 10

(*
    Exercise 4.2
    Declare function downto1: int -> int list such that the value of downto1 n is the list
[n; n − 1; . . . ; 1].
*)
(*
// Solution 1
let downto1 n = [n..(-1)..1]
downto1 5
*)

// Solution 2
let rec downto1 n = 
    match n with
        | 1 -> [1]
        | _ -> n :: (downto1 (n-1))
downto1 5

(*
    Exercise 4.3
*)
(*
// Solution 1, with list comprehension
let evenN n = [2..2..n*2]
evenN 5
evenN 11
*)

(*
// Solution 2, with a special kind of list_from_to function
let rec list_from_to_with_step (a,b,step) =
    if a > b then []
    else a :: list_from_to_with_step (a+step,b,step)
let evenN n = list_from_to_with_step (2,n*2,2)
evenN 5
evenN 11
*)

// Solution 3 
let isEven = fun x -> x % 2 = 0
let rec filter_list = function
    | [] -> []
    | x::xs -> if isEven x
                    then x::filter_list(xs)
                    else filter_list(xs)

let evenN n = 
    let full_list = list_from_to (1,n*2) in
    filter_list full_list

evenN 5
evenN 11

(*
    Exercise 4.4
    Test case 1 = [1;2;3;4;5]
    sum=1-2+3-4+5=3
    Test case 2 = [1;2;3;4;5;6]
    sum=1-2+3-4+5-6=-3
*)
(*
// Solution 1
let altsum list = 
  let rec altsum' sign = function
    | [] -> 0
    | x::xs -> (if sign then -1 else 1) * x + altsum' (not sign) xs
  altsum' false list
*)

// Solution 2
let altsum list = 
  let rec altsum' itemNumber = function
    | [] -> 0
    | x::xs -> int (((-1.) ** float itemNumber)) * x + altsum' (itemNumber+1) xs
  altsum' 0 list

altsum [1;2;3;4;5]
altsum [1;2;3;4;5;6]

(*
    Exercise 4.5
    rmodd [x0;x1;x2;x3; . . . ] = [x0;x2; . . . ]
*)
let rec rmodd = function
    | x0::x1::xs -> x0 :: rmodd xs
    | xs -> xs

rmodd [2;4]
rmodd [1;3;5]
rmodd [2;3;4;5]

(*
    Exercise 4.6
*)
let rec rmeven = function
    | x::xs -> if x % 2 = 0 then rmeven(xs) else x::rmeven(xs)
    | _ -> []

rmeven [2;4]
rmeven [1;3]
rmeven [2;3;4]

(*
    Exercise 4.7
    Test case 1 = 5 [1;5;3;5;5] -> 3
    Test case 2 = 5 [1;3] -> 0
    Test case 3 = 5 [-1;5;1] -> 1
*)
let rec multiplicity x = function
    | x'::xs' -> (if x' = x then 1 else 0) + multiplicity x xs' 
    | [] -> 0

multiplicity 5 [1;5;3;5;5]
multiplicity 5 [1;3]
multiplicity 5 [-1;5;1]

(*
    Exercise 4.8
*)
let rec splitter = function
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x0::x1::rest ->
        let (rFrst,rScnd) = splitter rest
        (x0::rFrst,x1::rScnd)

splitter []
splitter [1]
splitter [1;2]
splitter [1;2;3;4]
splitter [1;2;3;4;5]

(*
    Exercise 4.9
*)
let rec zip = function
    | ([],[]) -> []
    | (x::_,[]) -> failwith "zip: lists are not equal length"
    | ([],y::_) -> failwith "zip: lists are not equal length"
    | (x::xs,y::ys) -> (x,y)::(zip (xs,ys))

zip ([1],[2])
zip ([1;2],[3;4])
zip ([1],[2;3])
zip ([1;2],[3])

(*
    Exercise 4.10
*)
let rec prefix = function
    | (x::xs,y::ys) -> if x = y then prefix (xs,ys) else false
    | ([],_) -> true
    | _ -> false

prefix ([],[2])
prefix ([1],[1;2])
prefix ([1,2],[1,2])
prefix ([2],[])
prefix ([1],[2;3])

(*
    Exercise 4.11
*)
// .1
let rec count (list,elem) = 
    match list with
        | x::xs when x <= elem -> (if x = elem then 1 else 0) + count (xs,elem)
        | _ -> 0

count ([2;3;3;4;4;4],1)
count ([2;3;3;4;4;4],5)
count ([2;3;3;4;4;4],3)
count ([],3)

// .2
let rec insert (sourceList,newElement) = 
    match sourceList with
        | x::xs when x <= newElement -> x::insert(xs,newElement)
        | x::xs when x > newElement -> newElement::sourceList
        | _ -> [newElement]

insert ([3;4],2)
insert ([3;4],3)
insert ([3;4],5)

//.3
let rec intersect = function
    | (x::xs,y::ys) when x = y -> x::intersect (xs,ys)
    | ((x::_ as xs),y::ys) when x > y -> intersect (xs,ys)
    | (x::xs,(y::_ as ys)) when x < y -> intersect (xs,ys)
    | (_,_) -> []

intersect ([1;1;1;2;2],[1;1;2;4])
intersect ([1;2],[3;4])
intersect ([1;2;2],[2;3;4])

//.4, variant 1
// let rec plus = function
//     | (xs,y::ys) -> let xs2 = insert (xs,y) 
//                     plus (xs2,ys)
//     | (xs,[]) -> xs

// plus ([1;2],[2;3])
// plus ([1;1;2],[1;2;4]) // [1;1;1;2;2;4]

//.4, variant 2
let rec plus = function
    | (x::xs,y::ys) when x = y -> x::y::plus (xs,ys)
    | ((x::_ as xs),y::ys) when x > y -> y::plus (xs,ys)
    | (x::xs,(y::_ as ys)) when x < y -> x::plus (xs,ys)
    | (xs,ys) -> xs @ ys
    | _ -> []

plus ([1;1;2],[1;2;4]) // [1;1;1;2;2;4]

//.5
let rec minus = function
    | (x::xs,y::ys) when x = y -> minus (xs,ys)
    | (x::xs,(y::_ as ys)) when x < y -> x::minus (xs,ys)
    | ((x::_ as xs),y::ys) when x > y -> minus (xs,ys)
    | (xs,[]) -> xs
    | _ -> []

minus ([1;1;1;2;2],[1;1;2;3]) // [1;2]
minus ([1;1;2;3],[1;1;1;2;2]) // [3]
minus ([1;2;3],[4;5;6]) // [1;2;3]
minus ([1;2;6],[4;5;6]) // [1;2] 
minus ([1],[1]) // []
minus ([4;5;6],[1;2;3]) // [4;5;6]

(*
    Exercise 4.12
*)
let rec sum (p,list) = 
    // Variant 1
    // match list with
    //     | x::xs -> if p x then x + sum (p,xs) else sum (p,xs)
    //     | _ -> 0
    // Variant 2
    // match list with
    //     | x::xs when (p x) = true -> x + sum (p,xs)
    //     | x::xs when (p x) = false -> sum (p,xs)
    //     | _ -> 0
    // Variant 3
    match list with
        | x::xs when p x -> x + sum (p,xs)
        | x::xs -> sum (p,xs)
        | _ -> 0

let div3 = fun x -> x % 3 = 0
let div2 = fun x -> x % 2 = 0

sum (div3, [1;2;3;4;5;6]) // 9
sum (div2, [1;2;3;4;5;6]) // 12
sum ((fun x -> false), [1;2;3]) // 0

(*
    Exercise 4.13
*)
// .1
let min (a,b) = if a <= b then a else b

let rec findMinElem = function
    | x::xs when xs <> [] -> min (x,findMinElem(xs))
    | [x] -> x
    | _ -> failwith "unknow state"

findMinElem [1;0;-1]
findMinElem [5]
findMinElem []
findMinElem [1;2;3]

// .2
let rec deleteElem (n,list)=
    match list with
        | x::xs when x = n -> xs
        | x::xs -> x::deleteElem (n,xs)
        | [] -> []

deleteElem (5,[])
deleteElem (5,[5])
deleteElem (5,[1;5])
deleteElem (5,[5;1])
deleteElem (5,[1;5;2])

// .3
// Variant 1
// let rec sort list = 
//     if list = [] 
//     then 
//         []
//     else
//         let minElem = findMinElem list
//         let listWitoutMinElem = deleteElem (minElem,list)
//         minElem :: sort listWitoutMinElem
// Variant 2
let rec sort = function
    | xs when xs <> [] -> let minElem = findMinElem xs in
                          let listWitoutMinElem = deleteElem (minElem,xs) in
                          minElem :: sort listWitoutMinElem
    | _ -> []

    // | x::xs as xs' -> let minElem = findMinElem xs' in
    //               let listWitoutMinElem = deleteElem (minElem,xs') in
    //               minElem :: sort listWitoutMinElem
    // | _ -> []

sort [3;2;1]
sort [1;-2;2;-1;0;3]
sort [3;2;1;2;1]

(*
    Exercise 4.14
*)
let rec findMinElem = function
    | x::xs when xs <> [] -> Some (min (x, Option.get(findMinElem(xs))))
    | [x] -> Some x
    | _ -> None

(*
    Exercise 4.15
*)
// Solution 1, using example from section "Append and reverse. Two built-in functions" 
// for a list of int
let rec naiveRev xls =
    match xls with
    | [] -> []
    | x::xs -> naiveRev xs @ [x]

let rec revrev xls =
    match xls with
    | [] -> []
    | x::xs -> revrev xs @ [naiveRev x]

revrev [[1;2];[3;4;5]] // [[5;4;3];[2;1]]
revrev [[3];[2];[1]]

(*
    Exercise 4.16
*)
// .1
// Function add sequntially decremented value to each element of a list
let rec f = function
    | (x, []) -> [] 
    | (x, y::ys) -> (x+y)::f(x-1, ys)

f (3,[1;3;5;7;9])

// .2
// Function add new element with permutated items of pair after each element of source list 
let rec g = function
    | [] -> []
    | (x,y)::s -> (x,y)::(y,x)::g s

g [(1,2);(3,4);(5,6)]

// .3
// Function is mirroring elements of a list
let rec h = function
    | [] -> []
    | x::xs -> x::(h xs)@[x]

h [1;2;3]

(*
    Exercise 4.17
    Type of the function is ('a -> bool) -> 'a lsit -> 'a list
    Some elements of a list may be moved into the end of a list
*)
let rec p q = function
    | [] -> []
    | x::xs -> let ys = p q xs
               if q x then x::ys else ys@[x]

p (fun x -> x % 2 <> 0) [1;2;3;4;5] // [1;3;5;4;2]

(*
    Exercise 4.18
    Function returns an output list with elements converted with the function g. 
    Each new call of g is calling more and more g function on each element of source list. 
*)
let rec f g = function
    | [] -> []
    | x::xs -> g x :: f (fun y -> g(g y)) xs

f (fun x -> x + 1) [1;2;3;4;5] // [2;4;7;12;21]

