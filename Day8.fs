module Day8

let node = parseRegex "(\w+)\s=\s\((\w+),\s(\w+)" (fun a -> a[0],(a[1],a[2]))
let parse = both (Seq.head >> List.ofSeq) (Seq.skip 2 >> Seq.map node >> Map)
 
let step (m:Map<_,_>) p d = match d with
                            | 'L' -> m[p] |> fst
                            | 'R' -> m[p] |> snd
                            | _ -> failwith "oops"

let walk till m s = let repeat = Seq.replicate 999 >> Seq.concat
                    repeat >> Seq.scan (step m) s >> Seq.takeWhile (till>>not) >> Seq.length

let part1 (d,m) = walk ((=) "ZZZ") m "AAA" d

let part2 (d,m) = let endsIn c = Seq.rev >> Seq.head >> (=) c
                  let walkMany = flip (walk (endsIn 'Z') m) d
                  
                  m |> Map.keys|> Seq.filter (endsIn 'A') |> Seq.map (walkMany >> int64) |> Seq.reduce lcm

let Solve (xs:string list) = xs |> parse |> both part1 part2
