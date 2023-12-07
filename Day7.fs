module Day7

let cardValue = ['A'; 'K'; 'Q';'J';'T';'9';'8';'7';'6';'5';'4';'3';'2';'_'] |> Seq.rev |> Seq.mapi (fun i c -> (c,(i+1))) |> Map
let cardValue2 = Map.update cardValue 'J' (Const 1)

let hand cv (xs:char seq) = let values = xs |> Seq.map (Map.lookup cv)
                            let ranked = values |> (Seq.groupBy id >> Seq.map (mapSnd Seq.length) >> Seq.sortByDescending swap >> List.ofSeq)
                            ranked,values

let parse m = Seq.map (splitOn ' ' >> fun a -> hand m a[0], a[1] |> String.fromChars |> int)

let rh h = match List.map snd h with
           | [5]       -> 6
           | [4;1]     -> 5
           | [3;2]     -> 4
           | [3;1;1]   -> 3
           | [2;2;1]   -> 2
           | [2;1;1;1] -> 1
           | _         -> 0

let ranker ((l,cl),_) ((r,cr),_) = match compare ((rh l),(rh r)) with
                                   | 0  -> Seq.zip cl cr |> Seq.find (fun (a,b) -> a <> b) |> compare
                                   | x -> x 

let game = Seq.sortWith ranker >> Seq.mapi (fun i (_,s) -> (i+1)*s) >> Seq.sum

let part1 = parse cardValue >> game

let mergeJ ((h,x),v) = match h |> Seq.tryFind (fst >> (=) 1) with
                        | None -> (h,x),v
                        | Some (_,l) when l = 5 -> (h,x),v 
                        | Some (_,l) -> let c,n = List.find (fst >> (<>) 1) h
                                        let nh = (c,l+n) :: (h |> List.filter (fun (x,_) -> x <> 1 && x <> c))
                                        (nh,x),v 

let part2 = parse cardValue2 >> Seq.map mergeJ >> game

let Solve (xs:string seq) = xs |> both part1 part2
