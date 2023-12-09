module Day8

let node = parseRegex "(\w+)\s=\s\((\w+),\s(\w+)" (fun a -> a[0],(a[1],a[2]))
let parse = both (Seq.head >> List.ofSeq) (Seq.skip 2 >> Seq.map node >> Map)
 
let branch (m:Map<_,_>) p d = match d with
                              | 'L' -> m[p] |> fst
                              | 'R' -> m[p] |> snd

let walk p m s = let repeat = Seq.replicate 999 >> Seq.concat
                 repeat >> until p (branch m) s >> Seq.length

let part1 (d,m) = walk ((=) "ZZZ") m "AAA" d

let part2 (d,m) = let endsIn c = Seq.last >> (=) c
                  let walkGhost = flip (walk (endsIn 'Z') m) d
                  
                  m |> Map.keys|> Seq.filter (endsIn 'A') |> Seq.map (walkGhost >> int64) |> Seq.reduce lcm

let Solve : (string seq -> int*int64) = parse >> both part1 part2
