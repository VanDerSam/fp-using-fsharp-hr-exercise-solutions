(*
    Exercise 6.1
*)
type Fexpr = 
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr

let rec D = function
    | Const _ -> Const 0.0
    | X -> Const 1.0
    | Add(fe,ge) -> Add(D fe, D ge)
    | Sub(fe,ge) -> Sub(D fe, D ge)
    | Mul(fe,ge) -> Add(Mul(D fe, ge), Mul(fe, D ge))
    | Div(fe,ge) -> Div(Sub(Mul(D fe,ge), Mul(fe,D ge)),Mul(ge,ge))
    | Sin fe -> Mul(Cos fe, D fe)
    | Cos fe -> Mul(Const -1.0, Mul(Sin fe, D fe))
    | Log fe -> Div(D fe, fe)
    | Exp fe -> Mul(Exp fe, D fe)

D(Sin(Mul(X, X)))
let d1 = D(Mul(Const 3.0, Exp X))

let rec toString = function
    | Const x -> string x
    | X -> "x"
    | Add(fe1,fe2) -> "(" + (toString fe1) + ")" + " + " + "(" + (toString fe2) + ")"
    | Sub(fe1,fe2) -> "(" + (toString fe1) + ")" + " - " + "(" + (toString fe2) + ")"
    | Mul(fe1,fe2) -> "(" + (toString fe1) + ")" + " * " + "(" + (toString fe2) + ")"
    | Div(fe1,fe2) -> "(" + (toString fe1) + ")" + " / " + "(" + (toString fe2) + ")"
    | Sin fe -> "sin(" + (toString fe) + ")"
    | Cos fe -> "cos(" + (toString fe) + ")"
    | Log fe -> "log(" + (toString fe) + ")"
    | Exp fe -> "exp(" + (toString fe) + ")"

toString(Mul(Cos(Mul(X, X)),Add(Mul(Const 1.0, X), Mul(X, Const 1.0))))
toString(Add(Mul(X, Mul(X, X)) , Mul(X, X)))

toString d1

let rec red = function
    | Const x -> Const x
    | X -> X
    | Add(fe,ge) -> let (fe_,ge_) = (red fe, red ge) in 
                    match (fe_,ge_) with 
                    | (Const 0.0, _) -> ge_
                    | (_, Const 0.0) -> fe_
                    | (Const a, Const b) -> Const (a + b)
                    | _ -> Add(fe_, ge_)
    | Sub(fe,ge) -> Sub(red fe, red ge)
    | Mul(fe,ge) -> let (fe_,ge_) = (red fe, red ge) in 
                    match (fe_,ge_) with 
                    | (Const 1.0, _) -> ge_
                    | (_, Const 1.0) -> fe_
                    | (Const 0.0, _) -> Const 0.0
                    | (_, Const 0.0) -> Const 0.0
                    | (Const a, Const b) -> Const (a * b)
                    | _ -> Mul(fe_, ge_)
    | Div(fe,ge) -> Div(red fe,red ge)
    | Sin fe -> Sin (red fe)
    | Cos fe -> Cos (red fe)
    | Log fe -> Log (red fe)
    | Exp fe -> Exp (red fe)

red d1
toString (red d1)
// Before val it : string = "((0) * (exp(x))) + ((3) * ((exp(x)) * (1)))"
// After val it : string = "(3) * (exp(x))"

let rd1 = red (Add(Const 3.0, Const 0.0))
let rd2 = red (Mul(Const 1.0, Const 3.0))
let d2 = (Add((Mul(Const 1.0, X)), (Add(X, Const 0.0))))
toString d2
red d2
toString (red d2)
let d3 = Mul(Const 1.0, Mul(Const 1.0, Mul(Const 1.0, Const 5.0)))
toString (red d3)

let d4 = Sin(Mul(Const 1.0, Add(Const 0.0, X)))
toString d4
toString (red d4)

(*
    Exercise 6.2
*)
let rec toStringPostfix = function
    | Const x -> string x
    | X -> "x"
    | Add(fe1,fe2) -> (toStringPostfix fe1) + " " + (toStringPostfix fe2) + " +"
    | Sub(fe1,fe2) -> (toStringPostfix fe1) + " " + (toStringPostfix fe2) + " -"
    | Mul(fe1,fe2) -> (toStringPostfix fe1) + " " + (toStringPostfix fe2) + " *"
    | Div(fe1,fe2) -> (toStringPostfix fe1) + " " + (toStringPostfix fe2) + " /"
    | Sin fe -> (toStringPostfix fe) + " sin"
    | Cos fe -> (toStringPostfix fe) + " cos"
    | Log fe -> (toStringPostfix fe) + " log"
    | Exp fe -> (toStringPostfix fe) + " exp"

let testExpr1 = Mul(Cos(Mul(X, X)),Add(Mul(Const 1.0, X), Mul(X, Const 1.0)))
toStringPostfix(testExpr1)
let testExpr2 = Add(Mul(X, Mul(X, X)) , Mul(X, X))
toStringPostfix(testExpr2)
let testExpr3 = Add(Const 5.0, X)
toStringPostfix(testExpr3)
let testExpr4 = Add(Const 5.0, Add(X, Const 1.0))
toStringPostfix(testExpr4)

// (x + 7.0)
let testExpr5 = Add(X, Const 7.0)
toStringPostfix(testExpr5)
// (x + 7.0) ∗ (x − 5.0)
let testExpr6 = Mul(Add(Cos X, Const 7.0), Sub(Log X, Const 5.0))
toStringPostfix(testExpr6)

(*
    Exercise 6.4
*)
type BinTree<'a,'b> =
    | Leaf of 'a
    | Node of BinTree<'a,'b> * 'b * BinTree<'a,'b>

let tree1 = Node(Node(Leaf 4, 2, Leaf 5), 1, Node(Leaf 5, 3, Leaf 4))

// 6.4.1
let rec leafVals_ = function
    | Leaf x -> [x]
    | Node(tl,x,tr) -> (leafVals_ tl) @ (leafVals_ tr)

let leafVals t = Set.ofList (leafVals_ t)

let leafsForTree1 = leafVals tree1

let rec leafVals2 = function
    | Leaf x -> set [x]
    | Node(tl,x,tr) -> let leftSubTree = (leafVals2 tl) in
                       let rightSubTree = (leafVals2 tr) in
                       Set.union leftSubTree rightSubTree

let leafs2ForTree1 = leafVals2 tree1

// 6.4.2 Should be similar to the tree traverse in Section 6.3
let rec nodeVals = function
    | Leaf x -> set []
    | Node(tl,x,tr) -> let leftSubTree = (nodeVals tl) in
                       let rightSubTree = (nodeVals tr) in
                       Set.union (Set.union leftSubTree rightSubTree) (set [x])

let nodesForTree1 = nodeVals tree1
let tree2 = Node(tree1, 6, Leaf 5)
let nodesForTree2 = nodeVals tree2

// 6.4.3
let vals t = (leafVals t,nodeVals t)

vals tree2
vals tree1

let rec vals2 = function
    | Leaf x -> (set [x], set [])
    | Node(tl,x,tr) -> let (leftSubTreeLeafs, leftSubTreeNodes) = (vals tl) in
                       let (rightSubTreeLeafs, rightSubTreeNodes) = (vals tr) in
                       (Set.union leftSubTreeLeafs rightSubTreeLeafs, 
                        Set.union (Set.union leftSubTreeNodes rightSubTreeNodes) (set [x]))

vals2 tree2
vals2 tree1

(*
    Exercise 6.5
*)
type AncTree = | Unspec
               | Info of AncTree * string * AncTree

let ancTree1 = Info(Info(Info(Unspec,"Ivan",Unspec),"Anatoliy",Unspec),
                    "Sergey",
                    Info(Info(Unspec,"Emelian",Unspec),"Lubov",Info(Unspec,"Fedora",Unspec)))

let rec getMalesFemales = function
    | Unspec -> ([],[],[])
    | Info (lat,anc,rat) -> let (leftMales, leftFemales, leftChild) = getMalesFemales lat in
                            let (rightMales, rightFemales, rightChild) = getMalesFemales rat in
                            (leftMales @ leftChild @ rightMales, leftFemales @ rightFemales @ rightChild, [anc])

let maleAnc t = let (males, females, child) = getMalesFemales t in males
let femaleAnc t = let (males, females, child) = getMalesFemales t in females

maleAnc ancTree1
femaleAnc ancTree1

(*
    Exercise 6.6
*)
type BinTree<'a when 'a : comparison> =
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>

let rec add x t =
    match t with
    | Leaf -> Node(Leaf,x,Leaf)
    | Node(tl,a,tr) when x<a -> Node(add x tl,a,tr)
    | Node(tl,a,tr) when x>a -> Node(tl,a,add x tr)
    | _ -> t

let rec inOrder = function
    | Leaf -> []
    | Node(tl,x,tr) -> (inOrder tl) @ [x] @ (inOrder tr)

let t3 = Node(Node(Leaf, -3, Leaf), 0, Node(Leaf, 2, Leaf))
let t4 = Node(t3, 5, Node(Leaf, 7, Leaf))

let t5 = add 4 t4

inOrder t5

let rec delete x t =
    match t with
    | Leaf -> Leaf
    | Node(tl,a,tr) when x<a -> Node(delete x tl,a,tr)
    | Node(tl,a,tr) when x>a -> Node(tl,a,delete x tr)
    | Node(tl,a,tr) when x=a -> let trInList = inOrder tr in 
                                let newNode = List.fold (fun t e -> add e t) tl trInList in 
                                newNode

let t6 = delete 0 t5
inOrder t6

let t7 = delete 5 t5
inOrder t7

let t8 = delete 4 t5
inOrder t8

(*
    Exercise 6.11
*)
type ListTree<'a> = Node of 'a * (ListTree<'a> list)

let t7 = Node(7,[]) 
let t6 = Node(6,[])
let t5 = Node(5,[]) 
let t3 = Node(3,[])
let t2 = Node(2,[t5]) 
let t4 = Node(4,[t6; t7])
let t1 = Node(1,[t2; t3; t4])

let rec depthFirstFold f e (Node(x,ts)) =
    List.fold (depthFirstFold f) (f e x) ts

depthFirstFold (fun a x -> x::a) [] t1

(*
    Evaluation:

    t1 = Node(1,[Node (2, [Node (5, [])]); Node (3, []); Node (4, [Node (6, []); Node (7, [])])])

    depthFirstFold (fun a x -> x::a) [] t1
    = List.fold (depthFirstFold f) (f e x) ts
    = List.fold (depthFirstFold f) (f [] 1) [Node (2, [Node (5, [])]); Node (3, []); Node (4, [Node (6, []); Node (7, [])])]
    = List.fold (depthFirstFold f) [1] [Node (2, [Node (5, [])]); Node (3, []); Node (4, [Node (6, []); Node (7, [])])]
    = List.fold (depthFirstFold f) (f [1] 2) [Node (5, [])]
    = List.fold (depthFirstFold f) [2;1] [Node (5, [])]
    = List.fold (depthFirstFold f) (f [2;1] 5) []
    = List.fold (depthFirstFold f) [5;2;1] []
    = List.fold (depthFirstFold f) [5;2;1] [Node (3, []); Node (4, [Node (6, []); Node (7, [])])]
    = List.fold (depthFirstFold f) (f [5;2;1] 3) []
    = List.fold (depthFirstFold f) [3;5;2;1]
    = List.fold (depthFirstFold f) [3;5;2;1] [Node (4, [Node (6, []); Node (7, [])])]
    = List.fold (depthFirstFold f) (f [3;5;2;1] 4) [Node (6, [])]
    = List.fold (depthFirstFold f) [4;3;5;2;1] [Node (6, [])]
    = List.fold (depthFirstFold f) (f [4;3;5;2;1] 6) []
    = List.fold (depthFirstFold f) [6;4;3;5;2;1]
    ...
    = [7;6;4;3;5;2;1]
*)

(*
    Exercise 6.12
*)
// depthFirstFoldBack
let rec depthFirstFoldBack g (Node(x,ts)) e =
    g x (List.foldBack (depthFirstFoldBack g) ts e)

depthFirstFoldBack (fun x a -> x::a) t1 []

let rec breadthFirstFoldBackList f e ts =
    match ts with
    | [] -> e
    | (Node(x,ts))::rest -> breadthFirstFoldBackList f (f x e) (rest@ts)

let breadthFirstFold f e t = breadthFirstFoldBackList f e [t]

breadthFirstFold (fun x a -> x::a) [] t1

(*
    Exercise 6.9
    It's more convenient do this exepcise after 6.11 and 6.12
*)
// 6.9.1
type DepartmentTree1 = Department of string * (DepartmentTree1 list)

let dt1 = Department("1", [Department("2",[])])

// 6.9.2
type DepartmentTree<'a> = Department of 'a * (DepartmentTree<'a> list)

let dt2 = Department(("1",100.0), [Department(("1.1",200.0),[]); Department(("1.2", 50.0),[])])

// 6.9.3
// Depth first
let rec depthFirst (Department(x,ts)) =
    x :: (List.collect depthFirst ts)

depthFirst dt2
let getListOfPairs dt = depthFirst dt

// 6.9.4
// Construction of depthFirstFoldBack2 similar to depthFirstFoldBack
let rec depthFirstFoldBack2 g (Department(x,ts)) e =
    g x (List.foldBack (depthFirstFoldBack2 g) ts e)

depthFirstFoldBack2 (fun x a -> let (_,incomeGross) = x in a + incomeGross) dt2 0.0
let getTotalIncome dt = depthFirstFoldBack2 (fun x a -> let (_,incomeGross) = x in a + incomeGross) dt 0.0

// 6.9.5
// As combination of function from 6.9.2 and 6.9.3
let dt3 = Department(("1",100.0), [
                                   Department(("1.1",200.0), [
                                       Department(("1.1.1",30.0),[]); 
                                       Department(("1.1.2",40.0),[])
                                   ]); 
                                   Department(("1.2", 50.0),[
                                       Department(("1.2.1",10.0),[]); 
                                       Department(("1.2.2",20.0),[])
                                   ])
                                   ])

let rec depthFirst2 (Department(x,ts)) =
  let (name,_) = x in (name,(getTotalIncome (Department(x,ts)))) :: (List.collect depthFirst2 ts)

let getListOfTotalIncome dt = depthFirst2 dt

getListOfTotalIncome dt3

// Variant 2, with one pass
let rec depthFirst3 (Department(x,ts)) =
    let childTotalIncomes = (List.map depthFirst3 ts) in 
        let (name,grossIncome) = x in 
            let totalIncome = (name, (List.fold (fun a x -> let (totalIncome,_) = x in let (_,incomeGross) = totalIncome in a + (float)incomeGross) grossIncome childTotalIncomes)) in
                (totalIncome,
                 totalIncome :: List.collect (fun (_,b) -> b) childTotalIncomes)

let getListOfTotalIncome2 dt = let (_,result) = depthFirst3 dt in result

getListOfTotalIncome2 dt3

// 6.9.6
// Based on depthFirst
let format dt = 
    let rec depthFirstToString depth (Department(x,ts)) =
        let (name, _) = x in
            (String.replicate depth "\t") 
            + name 
            + "\n" 
            + (List.fold (fun a x -> a + x) "" (List.map (depthFirstToString (depth + 1)) ts))
    
    printfn "%s" (depthFirstToString 0 dt)

format dt3

