module Day2
                              
let parse xs = let hand = parseRegex "\s(\d+)\s(\w+)" (fun a -> (int a[0],a[1]))
               let g = splitOn ':' xs
               let gi = g[0] |> parseRegex "Game (\d+)" (fun a -> int a[0])
               let sec = g[1] |> splitOnAny [';';','] |> Seq.map hand
               (gi,sec)

let part1 =
    let limit = Map [("red",12); ("green",13); ("blue",14)]
    
    Seq.filter (snd >> Seq.forall (fun (n,c) -> n <= limit[c])) >> Seq.sumBy fst

let part2 =
    let power = Seq.groupBy snd >> Seq.map (snd >> Seq.maxBy fst >> fst) >> Seq.reduce (*)

    Seq.sumBy (snd >> power)

let Solve : (string seq -> int * int) = Seq.map parse >> both part1 part2
