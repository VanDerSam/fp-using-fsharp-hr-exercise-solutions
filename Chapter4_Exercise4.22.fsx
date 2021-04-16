type Polynomial = int list

(*
    Exercise 4.22, part 4
*)

let coefficientToString coefficient = function
    | 0 -> match coefficient with
           | c when c = 0 -> ""
           | _ -> string coefficient
    | _ -> let tmpString = match coefficient with
                           | c when c = 0 -> ""
                           | c when c < 0 -> string c
                           | c when c > 0 -> "+" + (string c)
                           | _ -> ""
           if tmpString = "+1" || tmpString = "-1" then string tmpString.[0] else tmpString

coefficientToString 0 2
coefficientToString -1 0
coefficientToString 2 1
coefficientToString -2 2
coefficientToString 1 3
coefficientToString 0 0

let intermediateToString = function
    | 0 -> ""
    | 1 -> "x"
    | n -> "x^" + (string n)

intermediateToString 0
intermediateToString 1
intermediateToString 3

let elementToString coefficient index = 
    let coeff = coefficientToString coefficient index in
    let intermed = intermediateToString index in
    match coeff with
    | "" -> ""
    | _ -> coeff + intermed

elementToString 1 3

let toString polym =
    let rec toStringInternal elementIndex = function
        | [] -> ""
        | x::xs -> elementToString x elementIndex + toStringInternal (elementIndex + 1) xs
    let preResult = toStringInternal 0 polym in
    if preResult <> "" && preResult.[0] = '+' then preResult.[1..] else preResult

let polym1 = [2;0;0;1] // 2 + x^3
toString polym1

let polym2 = [0] // 0
toString polym2

let polym3 = []
toString polym3

let polym4 = [-1]
toString polym4

let polym5 = [-1;-1;-1]
toString polym5

let polym5 = [2;-1;0;5]
toString polym5

// Ошибка. Этот случай не обрабатывается
let polym10 = [0;0;-1]
toString polym10

(*
    Exercise 4.22, part 1
*)
let rec MultByConst constant = function
    | [] -> []
    | x::xs -> (x * constant)::MultByConst constant xs

let polym6 = [-1;-1;-1]
toString polym6
toString (MultByConst -1 polym6)
toString (MultByConst -3 polym6)
let polym7 = [0;0;1;2]
toString polym7
toString (MultByConst 5 polym7) // = 5x^2+10x^3

(*
    Exercise 4.23, part 2
    The main idea of this task is detect the first non zero element. Insert in this position 0 element and concatenate the rest of list. For example,
    x * (1+2x^2) -> x+2x^3
    x * [1;0;2] -> [0;1]@Rest[0;2] -> [0;1;0;2]
*)
