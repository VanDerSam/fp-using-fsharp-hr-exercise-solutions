(*
    Section 4.6
    Example: Cash register
*)
type ArticleCode = string
type ArticleName = string
type Price = int // pr where pr >= 0
type Register = (ArticleCode * (ArticleName*Price)) list

let reg = [("a1",("cheese",25));
           ("a2",("herring",4));
           ("a3",("soft drink",5)) ]

type NoPieces = int // np where np >= 0
type Item = NoPieces * ArticleCode
type Purchase = Item list

let pur = [(3,"a2"); (1,"a1")]

type Info = NoPieces * ArticleName * Price
type Infoseq = Info list
type Bill = Infoseq * Price

([(3,"herring",12); (1,"cheese",25)],37)

let rec findArticle ac = function
    | (ac',adesc)::_ when ac=ac' -> adesc
    | _::reg -> findArticle ac reg
    | _ ->
        failwith(ac + " is an unknown article code")

let rec makeBill reg = function
    | [] -> ([],0)
    | (np,ac)::pur -> let (aname,aprice) = findArticle ac reg
                      let tprice = np*aprice
                      let (billtl,sumtl) = makeBill reg pur
                      ((np,aname,tprice)::billtl,tprice+sumtl)

makeBill reg pur

// Version 1, Chapter 5
type Register = Map<ArticleCode, ArticleName*Price>

let reg1 = Map.ofList reg

let rec makeBill1 reg = function
    | [] -> ([],0)
    | (np,ac)::pur ->
        match Map.tryFind ac reg with
        | Some(aname,aprice) ->
            let tprice = np*aprice
            let (infos,sumbill) = makeBill1 reg pur
            ((np,aname,tprice)::infos, tprice+sumbill)
        | None ->
            failwith(ac + " is an unknown article code")

makeBill1 reg1 pur

// Version 2, Chapter 5
let makeBill2 reg pur =
    let f (np,ac) (infos,billprice) =
        let (aname, aprice) = Map.find ac reg
        let tprice = np*aprice
        ((np,aname,tprice)::infos, tprice+billprice)
    List.foldBack f pur ([],0)

makeBill2 reg1 pur

type Purchase = Map<ArticleCode,NoPieces>

let purMap = Map.ofList [("a2",3); ("a1",1)]

let makeBill3 reg pur =
    let f ac np (infos,billprice) =
        let (aname, aprice) = Map.find ac reg
        let tprice = np*aprice
        ((np,aname,tprice)::infos, tprice+billprice)
    Map.foldBack f pur ([],0)

makeBill3 reg1 purMap

(*
    Exercise 5.8
*)
let makeBill4 reg pur =
    let f (infos,billprice) ac np =
        let (aname, aprice) = Map.find ac reg
        let tprice = np*aprice
        ((np,aname,tprice)::infos, tprice+billprice)
    Map.fold f ([],0) pur

makeBill4 reg1 purMap

(*
    Exercise 5.9
    Also it may be solved with List.fold
*)
let createPurchaseMap pur = 
    let reversedPur = List.map (fun (a,b) -> (b,a)) pur in
    Map.ofList reversedPur

let pur1 = createPurchaseMap pur

makeBill4 reg1 pur1

