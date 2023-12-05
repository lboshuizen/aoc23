module Day5

open FSharp.Collections.ParallelSeq

let parseMap = Seq.tail >> Seq.map (allInt64 >> fun a -> (a[1],a[0],a[2])) >> Seq.sortBy (fun (s,_,_) -> s)

let parse xs = let sect = splitOnEmpty xs //let seeds = Seq.head xs
               let seeds = List.head sect |> Seq.map allInt64 |> Seq.head |> Seq.ofArray
               let maps = List.tail sect |> Seq.map parseMap |> Array.ofSeq
               (seeds,maps)

let lookup m n = match PSeq.tryFind (fun (s,_,r) -> n >= s && n <= (s+r-1L)) m with
                       | Some (s,d,_) -> d + (n-s)
                       | None -> n
                      
let reduce m = Seq.map (lookup m)

let part1 (s,m) = Seq.fold (flip reduce ) s m |> Seq.min

// Brute force, saturates mine M1 but needs a lot (!!!!!) of time
let part2 (s,m) = let e = Seq.chunkBySize 2 s |> Seq.map (tuple >> fun (s,r) -> seq { [for s' in s..s+r-1L -> s']  }) |> Seq.concat
                  e |> PSeq.map (fun s -> part1 (s,m)) |> Seq.min

let Solve xs = xs |> parse |> both part1 part2
