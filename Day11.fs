module Day11

let parse = toGrid2d >> Seq.filter (snd >> (=) '#') >> Seq.map fst >> List.ofSeq

let empty f xs = xs |> Seq.map f |> Set |> Set.difference ([0..(Seq.maxBy f xs |> f)] |> Set) |> Seq.sortDescending

let expand sel map n xs = let fld xs c = let r,l = xs |> List.partition (sel >> (<) c)
                                         l @ (r |> List.map (map ((+) n)))
                          Seq.fold fld xs (empty sel xs)

let manhattan ((x,y),(x',y')) = abs (x-x') + abs (y-y')

let calc n = let cols = expand fst mapFst
             let rows = expand snd mapSnd
                
             cols n >> rows n >> combinations >> Seq.sumBy (manhattan>>int64)
    
let part1 = calc 1
let part2 = calc (1_000_000-1)

let Solve : (string seq -> int64*int64) = parse >> both part1 part2 //(9403026, 543018317006)
