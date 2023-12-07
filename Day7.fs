module Day7

let cardValue = ['A'; 'K'; 'Q';'J';'T';'9';'8';'7';'6';'5';'4';'3';'2';'_'] |> Seq.rev |> Seq.mapi (fun i c -> (c,(i+1))) |> Map
let cardValue2 = Map.update cardValue 'J' (Const 1)

let rank h = match List.map snd h with
             | [5]       -> 6
             | [4;1]     -> 5
             | [3;2]     -> 4
             | [3;1;1]   -> 3
             | [2;2;1]   -> 2
             | [2;1;1;1] -> 1
             | _         -> 0

let hand vt merge xs = let values = xs |> Seq.map (Map.lookup vt)
                       let r = values |> Seq.groupBy id |> Seq.map (mapSnd Seq.length) |> Seq.sortByDescending swap
                               |> List.ofSeq |> merge |> rank
                 
                       values, r 

let parse vt f = Seq.map (splitOn ' ' >> fun a -> hand vt f a[0], a[1] |> String.fromChars |> int)

let ranker ((cl,lr),_) ((cr,rr),_) = match compare (lr,rr) with
                                     | 0  -> Seq.zip cl cr |> Seq.find (fun (a,b) -> a <> b) |> compare
                                     | x -> x 

let game = Seq.sortWith ranker >> Seq.mapi (fun i (_,b) -> (i+1) * b) >> Seq.sum

let part1 = parse cardValue id >> game

let mergeJ h = match h |> Seq.tryFind (fst >> (=) 1) with
               | None -> h
               | Some (_,l) when l = 5 -> h 
               | Some (_,l) -> let c,n = List.find (fst >> (<>) 1) h
                               let nh = (c,n+l) :: (h |> List.filter (fun (x,_) -> x <> 1 && x <> c))
                               nh 

let part2 = parse cardValue2 mergeJ >> game

let Solve : (string seq -> int*int) = both part1 part2 // (250957639, 251515496)

