module Day3

let parse = (toGrid2d >> Seq.filter (snd >> (<>) '.') >> Map)

let posIsDigit m xy = match Map.tryFind xy m with
                      | Some v when isDigit v -> true
                      | _ -> false

let touchNumber m = Seq.map (fun origin -> around origin |> Seq.filter (posIsDigit m) |> Seq.map (fun d -> (origin,d))) >> Seq.concat

let gridSize = 142

let partNumber g (c,row) =
    match Map.containsKey (c,row) g with
    | false -> None
    | _ -> let get p = g[p]
           let numberFrom = List.map get >> seq2i
           let pts = (List.takeWhile (posIsDigit g) ([for x' in 0..c-1 do (x',row)] |> List.rev) |> List.rev)
                      @ [(c,row)]
                      @ List.takeWhile (posIsDigit g) [for x' in c+1..gridSize do (x',row)]
           Some (delete g pts, numberFrom pts) 

let collect (m,xs) (o,xy) = match partNumber m xy with
                            | None -> (m,xs)
                            | Some (m',pn) -> (m',(o,pn)::xs)

let collectReduce g = Seq.fold collect (g,[]) >> snd
             
let part1  g = let symbols = Map.filter (fun _ v -> isDigit v |> not) >> Map.keys
              
               g |> symbols |> touchNumber g |> collectReduce g |> Seq.sumBy snd

let part2 g = let gears = Map.filter (fun _ v -> v = '*') >> Map.keys
              let ratio = Seq.groupBy fst >> Seq.filter (snd >> Seq.length >> (=) 2) >> Seq.map (snd >> Seq.map snd >> Seq.reduce (*))
              
              g |> gears |> touchNumber g |> collectReduce g
                |> ratio |> Seq.sum

let Solve : (string seq -> int * int) = parse >> both part1 part2
