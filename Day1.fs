module Day1

let isDigit c = System.Char.IsDigit c
let asInt (c:char) = int c - int '0'

let parse = List.map(Seq.filter( isDigit ) >> Seq.map asInt >> Seq.toList)

let firstLast = let num (x,y) = 10 * x + y         
                function
                | [x] -> num (x,x)
                | [] -> 0
                | x::xs -> num (x,List.last xs) 

let first (s:string) (i:string) = s.IndexOf(i)
let last (s:string) (i:string) = s.LastIndexOf(i)

let substitutes = [("one","1"); ("two","2"); ("three","3"); ("four","4"); ("five","5"); ("six","6"); ("seven","7"); ("eight","8"); ("nine","9")]

let replace index sel (s:string) = List.map (fun (on,r) -> (index s on,r) ) substitutes
                                   |> List.filter (fun (x,_) -> x <> -1)
                                   |> function
                                      | [] -> s
                                      | l -> l |> sel fst |> s.Insert

let part1 = parse >> List.map firstLast >> List.sum 

let part2 =
    let replFst = replace first List.minBy
    let replLst = replace last List.maxBy
    List.map (replFst >> replLst) >> part1

let Solve = both part1 part2
