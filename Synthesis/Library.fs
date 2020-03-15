module Synthesis

let abelar a =
        match (a>12 && a<3097 && a%12=0) with
        | true  -> true
        | false -> false

let area b h = 
        match (b<0.0 || h<0.0) with
        | true -> failwith "Wrong"
        | _ -> 0.5*b*h

let zollo a = 
   match a<0 with
   | true -> a * -1
   | _ -> a*2

let min a b=
        match a<b with
        | true -> a
        | _ -> b

let max a b =
        match a>b with
        | true -> a
        | _ -> b

let ofTime a b c = a*3600 + b*60 + c

let toTime a = 
     let hours = a/3600
     let minutes = a%3600/60
     let seconds = a%3600%60
     match a<0 with
     | true -> (0,0,0)
     |false -> (hours,minutes,seconds)

let digits a = 
    let rec ans b c = 
        match ((b <10 && b>=0) || (b> -10 && b<0)) with
        | true -> c + 1
        | false -> ans (b/10) (c+1)
    ans a 0
let minmax a =
    let b,c,d,e = a
    ( min ( min b c) (min d e )  , max ( max b c ) (max d e) )

let isLeap a = 
        match a<1582 with
        |true -> failwith"Invalid"
        |false -> 
            match ((a%4=0 && a%100<>0) || a%400=0) with
            |true -> true
            |false -> false

let month a =
   let days = (31,28,31,30,31,30,31,31,30,31,30,31)
   match a with
   | 1 -> ("January",31)  
   | 2 -> ("February",28)
   | 3 -> ("March",31)
   | 4 -> ("April",30)
   | 5 -> ("May",31)
   | 6 -> ("June",30)
   | 7 -> ("July",31)
   | 8 -> ("August",31)
   | 9 -> ("September",30)
   | 10 -> ("October",31)
   | 11 -> ("November",30)
   | 12 -> ("December",31)
   | _ -> failwith"Wrong"

let toBinary a :string =
        let rec changeBin b c =
            match b with
            | 0 | 1 -> string b + c
            |_ ->
                match (b%2=0) with
                | true -> changeBin (b/2) ("0"+c)
                | false -> changeBin (b/2) ("1"+c)
        match a<0 with
        |true -> failwith"Invalid"
        |false -> changeBin a ""

let bizFuzz a =
    let rec counter b (three,five,both)=
             match b<=0 with
             |true -> (three,five,both)
             |false ->
                match (b%3=0 ,b%5=0 ,(b%3=0 && b%5=0)) with
                |(true,false,false) -> counter (b-1) (three+1,five,both)
                |(false,true,false) -> counter (b-1) (three,five+1,both)
                |(true,true,true) -> counter (b-1) (three+1,five+1,both+1)
                |_ -> counter (b-1) (three,five,both)
    counter a (0,0,0)

let monthDay _ _ =
    failwith "Not implemented"

let coord __ =
    failwith"Not implemented"
(*
    let (c,d)  = a
    let (e,f) = b
    let dist = ((c-e)**2.0 + (d-f)**2.0)**0.5
    let within g h i j = *)
        