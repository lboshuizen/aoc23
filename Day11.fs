module Day11

let parse = toGrid2d >> Seq.filter (snd >> (=) '#') >> Seq.map fst >> List.ofSeq

let empty f xs = xs |> Seq.map f |> Set |> Set.difference ([0..(Seq.maxBy f xs |> f)] |> Set) |> Seq.sortDescending

let expand sel map n xs = let fld xs (c:int) = let h,t = xs |> List.partition (sel >> (<) c)
                                               t @ (h |> List.map (map ((+) n)))
                          Seq.fold fld xs (empty sel xs)

let manhattan ((x,y),(x',y')) = abs (x-x') + abs (y-y')

let calc n = let cols = expand fst mapFst
             let rows = expand snd mapSnd
                
             cols n >> rows n >> List.map (biMap int64) >> combinations >> Seq.sumBy manhattan
    
let part1 = calc 1
let part2 = calc (1_000_000-1)

let Solve (xs:string seq) = xs |> parse |> both part1 part2
