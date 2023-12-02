module Day2
                              
let line xs = let hand = parseRegex "\s(\d+)\s(\w+)" (fun a -> (int a[0],a[1]))
              let g = splitOn ':' xs
              let sec = g[1] |> splitOn ';' |> Array.map (splitOn ',' >> Array.map hand >> Seq.ofArray) |> Seq.concat
              let gi = g[0] |> splitOn ' ' |> fun a -> int a[1]
              (gi,sec)
 
let parse  = Seq.map line

let part1 =
    let limit = Map.ofList [("red",12); ("green",13); ("blue",14)]
    
    Seq.filter (snd >> Seq.forall (fun (n,c) -> n <= limit[c])) >> Seq.sumBy fst

let part2 =
    let power = Seq.groupBy snd >> Seq.map (snd >> Seq.maxBy fst >> fst) >> foldl (fun a c -> a * c) 1

    Seq.sumBy (snd >> power)

let Solve : (string seq -> int * int) = parse >> both part1 part2
