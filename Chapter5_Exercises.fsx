(*
    Exercise 5.1
    5.1 Give a declaration for List.filter using List.foldBack.
*)
let filterList f list = 
    List.foldBack (fun x result -> if (f x) then x::result else result) list []

// Test 1
List.filter (fun x -> x % 3 = 0) [1;-3;6;18;2;5]
filterList (fun x -> x % 3 = 0) [1;-3;6;18;2;5]

// Test 2
List.filter (fun x -> x % 3 = 0) []
filterList (fun x -> x % 3 = 0) []

// Test 3
List.filter (fun x -> x % 3 = 0) [1]
filterList (fun x -> x % 3 = 0) [1]

// Test 4
List.filter (fun x -> x % 3 = 0) [3]
filterList (fun x -> x % 3 = 0) [3]

(*
    Exercise 5.2
    5.2 Solve Exercise 4.15 using List.fold or List.foldBack.
    Theory is on page 99
    let rev xs = List.fold (fun rs x -> x::rs) [] xs;;
*)

let revrev xs = 
    let rev xs = List.fold (fun rs x -> x::rs) [] xs in
    List.fold (fun rs x -> (rev x)::rs) [] xs

revrev [[1;2];[3;4;5]] // [[5;4;3];[2;1]]
revrev [[3];[2];[1]]
revrev [[2;1];[4;3];[];[1];[6;5]]

(*
    Exercise 5.3
    5.3 Solve Exercise 4.12 using List.fold or List.foldBack.

*)
let div3 = fun x -> x % 3 = 0
let div2 = fun x -> x % 2 = 0

let sum (p,list) = 
    List.fold (fun sum x -> if p x then sum + x else sum ) 0 list

sum (div3, [1;2;3;4;5;6]) // 9
sum (div2, [1;2;3;4;5;6]) // 12
sum ((fun x -> false), [1;2;3]) // 0

(*
    Exercise 5.4
*)
let downto1 f n e = 
    match n with 
    | t when t > 0 -> List.foldBack f [1..n] e
    | _ -> e

downto1 (fun x e -> e + x) 5 0 // 5+4+3+2+1=15
downto1 (fun x e -> 0) -5 6 // 6

// Fibonacci function
let factorial n = downto1 (fun x e -> e * x) n 1

factorial 5 // 120

(*
 Declare a function that builds the list [g(1), g(2), . . . , g(n)] for a function g
and an integer n.
*)
let listBuilder g n = downto1 (fun x e -> g(x)::e) n []

listBuilder (fun x -> x + 1) 5 // [2;3;4;5;6]
listBuilder (fun x -> x) 10
listBuilder (fun x -> x * x) 6

(*
    Exercise 5.5 in a separated file.
*)

(*
    Exercise 5.6.1
    Suppose that set A = {1,2,...,n} and set B = {"1", "2", ... , "n"}
*)
let dom r = Set.map (fun (a,_) -> a) r

// Tests
dom (set [(1,"2");(3,"4")]) // set [1;3]
dom (set [("a",4);("1",2)]) // set ["1";"a"]
dom (Set.empty) // set []

(*
    Exercise 5.6.2
*)
let rng r = Set.map (fun (_,b) -> b) r

// Tests
rng (set [(1,"2");(3,"4");(1,"5")]) // set ["2";"4";"5"]
rng (set [("a",4);("1",2)]) // set [2;4]
rng (Set.empty) // set []

(*
    Exercise 5.6.3
*)
let apply r a = Set.fold (fun result (x1,x2) -> if x1 = a then Set.add x2 result else result ) Set.empty r

apply (set [(1,2);(3,4);(5,6)]) 1 // set [2]
apply (set [(1,2);(2,4);(1,6)]) 1 // set [2;6]
apply (Set.empty) // set []

let apply2 r a = 
    let filteredR = Set.filter (fun (x,_) -> x = a) r in
        rng filteredR

apply2 (set [(1,2);(3,4);(5,6)]) 1 // set [2]
apply2 (set [(1,2);(2,4);(1,6)]) 1 // set [2;6]
apply2 (Set.empty) // set []

(*
    Exercise 5.6.4
*)
let symmetricClosure s = 
    let invertedS = Set.map (fun (a,b) -> (b,a)) s in
    Set.union s invertedS

symmetricClosure (set [(1,2);(4,3)]) // set [(1,2);(2,1);(3;4);(4,3)]

let symmetricClosure2 s = 
    Set.fold (fun result (a,b) -> Set.add (a,b) (Set.add (b,a) result) ) Set.empty s

symmetricClosure2 (set [(1,2);(4,3)]) // set [(1,2);(2,1);(3;4);(4,3)]

(*
    Exercise 5.8, Exercise 5.8 are in a separate file
*)

