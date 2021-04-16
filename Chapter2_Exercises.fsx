(*
    Exercise 2.1
    let f n = (n % 2 = 0 || n % 3 = 0) && n % 5 <> 0
    f(24) = true
    f(27) = true
    f(29) = false
    f(30) = false
*)
let f n = (n % 2 = 0 || n % 3 = 0) && n % 5 <> 0
f 24
f 27
f 29
f 30

(*
    Exercise 2.2
*)
let rec pow  = function
          | (s,0) -> ""
          | (s,n) -> s + pow(s,n-1)
pow ("a",5)
pow ("ab",3)

(*
    Exercise 2.3
    Concrete type for argument str is required.
*)
let isIthChar (str:string,i,ch) = str.[i] = ch
isIthChar ("abc", 2, 'c')
isIthChar ("abc", 2, 'b')

(*
    Exercise 2.4
*)
let rec occFromIth (str, i, ch) = 
        if i >= String.length str 
          then 0
          else 
            if isIthChar(str, i, ch) 
              then 1 + occFromIth(str, (i + 1), ch)
              else occFromIth(str, (i + 1), ch)
//           012345678
occFromIth ("abc1c2c3c", 3, 'c')
occFromIth ("abc1D2d3c", 3, 'c')
occFromIth ("abccD2d3DD", 3, 'c')

(*
    Exercise 2.5
    I can use the function from exercise 2.4!
*)
let occInString (str, ch) = occFromIth(str, 0, ch)
occInString ("abc1c2c3c", 'c') // 4
occInString ("cb-1-2+3c", 'c') // 2

(*
    Exercise 2.6
*)
let notDivisible (d,n) = n % d <> 0
notDivisible(2,5)
notDivisible(3,9)

(*
    Exercise 2.7
*)
let rec test (a,b,c) = 
   if a > b then true
   else notDivisible(a,c) && test(a+1,b,c)

test(2,10,11) // true
test(2,11,11) // false
test(7,9,12) // false

let prime n = test(2,n-1,n)
prime 1
prime 2
prime 3
prime 4
prime 16
prime 17
prime 90
prime 89
prime 88

let rec nextPrime n = 
  if prime(n + 1) then n + 1
  else nextPrime(n + 1)

nextPrime 15
nextPrime 67
nextPrime 31

(*
    Exercise 2.8
    In this exercise to use pattern matchin with more complex subconditions.
    Section 2.10 helps with example of pattern matching
*)
let rec bin = function
  | (n,0) -> 1
  | (n,k) when n = k -> 1
  | (n,k) -> bin (n-1,k-1) + bin (n-1,k)
bin (2,1)
bin (4,2)
bin (7,3) // 35

(*
    Exercise 2.9
    For section 2.6
    1. int * a' -> a' / int * int -> int
    2. for one argument of tuple type
    3. f (2,3):
    f (2,3) = f (2-1, 2*3) = f (1, 6) = f (0, 1 * 6) = f (0, 6) = 6
    4. f (x,y) = y * x * (x - 1) * ... * (x - x) = y * x!
*)
let rec f = function
    | (0,y) -> y
    | (x,y) -> f(x-1, x*y)

f (2,3)

(*
    Exercise 2.10
    1. bool * int -> int
    2. test(false, fact(-1))? An Exception
    3. if false then fact -1 else 0? 0
*)
let test(c,e) = if c then e else 0

let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)

test(false, fact(-1))
if false then fact -1 else 0

(*
    Exercise 2.11
    1. VAT: int -> float -> float
    2. unVAT: int -> float -> float
    if x' = 100+n%
    then x = 100%
    x = x' * 100% / 100+n%
*)
let VAT n (x:float) = let fn = float n in x + x * fn * 0.01

VAT 10 0.2

let unVAT n (x:float) = let fn = float n in x * 100.0 / (100.0 + fn)

unVAT 10 (VAT 10 0.2)
unVAT 50 (VAT 50 10.0)

let VAT10 = VAT 10
VAT10 10.0

(*
    Exercise 2.12
    Declare a function min of type (int -> int) -> int. The value of min(f) is the smallest
natural number n where f(n) = 0 (if it exists).
*)
let min1 f = let n = 1000 in
             let min (a,b) = if a <= b then a else b in
             let rec findMin = function
               | (_,minN,0) -> minN
               | (minF, minN, n) -> if f n < minF then findMin (f n, n, n-1)
                                                  else findMin (minF, minN, n-1) in
             findMin (n,n,n)

// min f = 3
let testF1 = function
  | 0 -> 5
  | 1 -> 4
  | 2 -> 3
  | 3 -> 2
  | _ -> 1000

// min f = 1
let testF2 = function
  | 0 -> -5
  | 1 -> -10
  | 2 -> 3
  | 3 -> 20
  | _ -> 1000

min1 testF1
min1 testF2

let min2 f = let n = 1000 in
             let rec findMin = function
               | (minN,0) -> minN
               | (minN, n) -> if f n = 0 then findMin (n, n-1)
                                         else findMin (minN, n-1) in
             findMin (n,n)

let testF3 = function
  | 0 -> -5
  | 1 -> 0
  | 2 -> 2
  | 3 -> 0
  | _ -> 1000

min2 testF3 // 1

let testF4 = function
  | 0 -> -5
  | 1 -> -1
  | 2 -> 2
  | 3 -> 5
  | _ -> 1000

min2 testF4 // 1

(*
    Exercise 2.13
*)
let curry f = fun a b -> f (a,b)
let uncurry f = fun (a,b) -> (f a b)

let add1 (a,b) = a + b
let add2 a b = a + b

curry add1
uncurry add2
curry add2 // error
