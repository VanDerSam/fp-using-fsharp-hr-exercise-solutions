(*
    Exercise 3.1
    Стандартный оператор сравнения < будет сравнивать лексикографически компоненты слева направо. 
    Поэтому в первом тесте это работать не будет.
    Вариант 1 - просто перекомпоновать кортежи там, чтобы шло сравнение в лексикографическом формате.
    В формате Запись
    Если определить компонет f впереди всех, то лексикографически можно сравнивать и по тестовому примеру.

*)
(11,59,"AM") < (1,15,"PM")

let dayBefore (h1,m1,f1) (h2,m2,f2) = (f1,h1,m1) < (f2,h2,m2) 

dayBefore (11,59,"AM") (1,15,"PM") // true
dayBefore (10,40,"AM") (10,41,"AM") // true
dayBefore (10,40,"AM") (10,40,"AM") // false
dayBefore (9,40,"AM") (10,40,"AM") // true
dayBefore (9,39,"PM") (10,40,"AM") // false

type Date = { hours: int; minutes: int; f: string }
{hours = 11; minutes = 59; f = "AM"} < {hours = 1; minutes = 15; f = "PM"}
let dayBefore2 a b = dayBefore (a.hours,a.minutes,a.f) (b.hours,b.minutes,b.f)
dayBefore2 {hours = 11; minutes = 59; f = "AM"} {hours = 1; minutes = 15; f = "PM"}

//let (.||.) p q = (p || q) && not(p && q);;
let (.<.) a b = dayBefore (a.hours,a.minutes,a.f) (b.hours,b.minutes,b.f)
{hours = 11; minutes = 59; f = "AM"} .<. {hours = 1; minutes = 15; f = "PM"}
{hours = 10; minutes = 40; f = "AM"} .<. {hours = 10; minutes = 40; f = "AM"}

(*
    Exercise 3.2
    Infix operator theory in Section 2.9
    Последнее условие с удобным представлением из tuple pattern мне не понятно.
    (pounds, shillings, pence)
*)
//let (.+) (po1,sh1,pe1) (po2,sh2,pe2) = (po1+po2,sh1+sh2,pe1+pe2)
let (.+) (po1,sh1,pe1) (po2,sh2,pe2) = 
  let penseSum = pe1 + pe2 
  let newPense = penseSum % 12 
  let shillingsSum = sh1 + sh2 + penseSum / 12
  let newShillings = shillingsSum % 20
  let newPounds = po1 + po2 + shillingsSum / 20 
  (newPounds, newShillings, newPense)

(1,1,1) .+ (2,2,2)

(1,19,10) .+ (1,0,10)

let (.-) (po1,sh1,pe1) (po2,sh2,pe2) = 
  let penseSub,shilling = if pe1 < pe2
                               then (pe1 + 12 - pe2, 1)
                               else (pe1 - pe2, 0)
  let shillingsSub,pound = if sh1 - shilling < sh2
                             then (sh1 - shilling + 20 - sh2, 1)
                             else (sh1 - shilling - sh2, 0)
  let poundsSub = po1 - pound - po2
  (poundsSub,shillingsSub,penseSub)

(1,10,10) .- (0,5,5)
(1,10,10) .- (0,5,11)
(1,10,10) .- (0,10,11)
let pounds,shillings,pense = (1,10,10) .- (1,10,11)

type Currency = { pounds: int; shillings: int; pense: int }

let toPense = fun a -> a.pounds * 240 + a.shillings * 12 + a.pense
let fromPense x = 
  let inPense = x
  let pense = inPense % 12
  let inShillings = inPense / 12
  let shillings = inShillings % 20
  let pounds = inShillings / 20
  (pounds,shillings,pense) 

let (.+.) (a:Currency) (b:Currency) = 
  let aInPense = toPense a
  let bInPense = toPense b
  let (pounds,shillings,pense) = fromPense (aInPense + bInPense)
  {pounds = pounds; shillings = shillings; pense = pense}

let (.-.) (a:Currency) (b:Currency) = 
  let aInPense = toPense a
  let bInPense = toPense b
  let sign = if aInPense < bInPense then -1 else 1 
  let (pounds,shillings,pense) = fromPense (aInPense - bInPense)
  {pounds = sign * pounds; shillings = shillings; pense = pense}

{pounds=1;shillings=1;pense=1} .+. {pounds=2;shillings=2;pense=2}
{pounds=1;shillings=19;pense=10} .+. {pounds=1;shillings=0;pense=10}

{pounds=1;shillings=19;pense=10} .-. {pounds=1;shillings=0;pense=10}

(*
    Exercise 3.3
    Represent a complex number as a tuple pair.
*)
let (.+.+) (a,b) (c,d) = (a+c,b+d)
let (.*.*) (a,b) (c,d) = (a*c-b*d,b*c+a*d)
let (~-.) (a,b) = (-a,-b)
-. (1,2)

let (~%) (a,b) = let divisor = (a*a+b*b) in (a/divisor, -b/divisor)
let (.-.-) (a,b) (c,d) = let (_c,_d) = -. (c,d) in (a,b) .+.+ (_c,_d)
let (././) (a,b) (c,d) = let (_c,_d) = % (c,d) in (a,b) .*.* (_c,_d)

(*
    Exercise 3.4
*)
type StraightLine = float * float

let toString (ln:StraightLine) = let (a,b) = ln in (string a) + "x + " + (string b)
toString (5.,8.)
let mirrorByX (a,b) = fun x -> -(a * x + b)
let mirrorByY (a,b) = fun x -> a * (-x) + b

let mirrorByX_ = mirrorByX (1,2)

(*
    Exercise 3.5
    1. The quesection is which type to use for NoRoot part?
*)
type Solution = 
  | TwoRoots of float*float
  | OneRoot of float
  | NoRoot of unit

let solve (a,b,c) =
    let d = b*b-4.0*a*c
    if d < 0.0 || a = 0.0
      then NoRoot ()
      else 
        if d = 0.0
          then OneRoot (-b/(2.0*a)) 
          else TwoRoots ((-b + sqrt d)/(2.0*a),(-b - sqrt d)/(2.0*a))

solve(1.0, 0.0, 1.0)
solve(1.0, 1.0, -2.0)
solve(2.0, 8.0, 8.0)

(*
    Exercise 3.6
    Tests and code from Exercise 3.1
*)
type TimeFormat = 
  | AM
  | PM

let dayBefore (h1,m1,f1) (h2,m2,f2) = (f1,h1,m1) < (f2,h2,m2) 

dayBefore (11,59,AM) (1,15,PM) // true
dayBefore (10,40,AM) (10,41,AM) // true
dayBefore (10,40,AM) (10,40,AM) // false
dayBefore (9,40,AM) (10,40,AM) // true
dayBefore (9,39,PM) (10,40,AM) // false

(*
    Exercise 3.7
    Example of guared patterns from Section 2.10
    let ordText x y = match compare x y with
        | t when t > 0 -> "greater"
        | 0 -> "equal"
        | _ -> "less";;
*)
type Shape = 
  | Circle of float
  | Square of float
  | Triangle of float*float*float

let isShape = function
    | Circle r -> r > 0.0
    | Square a -> a > 0.0
    | Triangle(a,b,c) ->
        a > 0.0 && b > 0.0 && c > 0.0
        && a < b + c && b < c + a && c < a + b

let area x =
    if not (isShape x)
    then failwith "not a legal shape" raise
    else match x with
        | Circle r -> System.Math.PI * r * r
        | Square a -> a * a
        | Triangle(a,b,c) ->
            let s = (a + b + c)/2.0
            sqrt(s*(s-a)*(s-b)*(s-c))

area (Triangle(3.0,4.0,5.0))
area (Triangle(3.0,4.0,7.5))

let area2 x = 
    match isShape x with
        | t when t = false -> failwith "not a legal shape" raise
        | _ -> match x with
                | Circle r -> System.Math.PI * r * r
                | Square a -> a * a
                | Triangle(a,b,c) ->
                    let s = (a + b + c)/2.0
                    sqrt(s*(s-a)*(s-b)*(s-c))

area2 (Triangle(3.0,4.0,5.0))
area2 (Triangle(3.0,4.0,7.5))

let area3 x = 
    match x with
        | Shape when isShape x = false -> failwith "not a legal shape" raise
        | Circle r -> System.Math.PI * r * r
        | Square a -> a * a
        | Triangle(a,b,c) ->
            let s = (a + b + c)/2.0
            sqrt(s*(s-a)*(s-b)*(s-c))

area3 (Triangle(3.0,4.0,5.0))
area3 (Triangle(3.0,4.0,7.5))
