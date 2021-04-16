(*
    Exercise 1.1
*)
let g n = n + 4
g (5)

(*
    Exercise 1.2
    let h (x,y) = System.Math.Sqrt( x * x + y * y) 
*)
let h (x,y) = System.Math.Sqrt( x * x + y * y)
h (3.,4.)

(*
    Exercise 1.3
*)
let g = fun n -> n + 4
g (5)
let h = fun (x,y) -> System.Math.Sqrt( x * x + y * y)
h (3.,4.)

(*
    Exercise 1.4
    1+2+3+4=10
*)
let rec f = function
        | 1 -> 1
        | n -> n + f(n-1)
f (4)

(*
    Exercise 1.5
*)
let rec fib = function 
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n-1) + fib (n - 2)
fib 4

(*
    Exercise 1.6
    sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
    Example m=2, n=5. Manually calculated value for 5 = 2 + 3 + 4 + 5 + 6 + 7 = 27
*)
let rec sum = function
        | (m,0) -> m
        | (m,n) -> m + n + sum (m, n - 1)
sum (2,5)

(*
    Exercise 1.7
    (System.Math.PI, fact -1) - pair/tuple, no infinite evalulation !
    fact(fact 4) - int
    power(System.Math.PI, fact 2) - float
    (power, fact) - pair/tuple of functions
*)
let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1);;  

let rec power = function
    | (x,0) -> 1.0                //  (1)
    | (x,n) -> x * power(x,n-1);; //  (2)

(System.Math.PI, fact -1)
fact(fact 4)
power(System.Math.PI, fact 2)
(power, fact)

(*
    Exercise 1.8
    let a = 5;;
    let f a = a + 1;;
    let g b = (f b) + a;;
    f 3;;
    g 3;;
    Environment:
    a -> 5
    f -> function a->a+1
    g -> function b->f(b)+5
    Evaluation:
    f 3 => 3 + 1 => 4
    g 3 => f (3) + 5 => (3+1) + 5 => 4 + 5 => 9
*)
let a = 5
let f a = a + 1
let g b = (f b) + a
f 3
g 3
