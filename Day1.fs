module Day1

let first (s:string) (i:string) = s.IndexOf(i)
let last (s:string) (i:string) = s.LastIndexOf(i)

let substitutes = [("one","1"); ("two","2"); ("three","3"); ("four","4"); ("five","5"); ("six","6"); ("seven","7"); ("eight","8"); ("nine","9")]

let calibrate = both (Seq.find isDigit) (Seq.findBack isDigit) >> fun (cx,cy) -> 10 * (a2i cx) + (a2i cy)

let part1 = Seq.map calibrate >> Seq.sum 

let replace index selBy (s:string) = List.map (fun (on,r) -> (index s on,r) ) substitutes
                                     |> List.filter (fst >> (<>) -1)
                                     |> function
                                        | [] -> s
                                        | xs -> xs |> selBy fst |> s.Insert

let part2 = Seq.map (replace first List.minBy >> replace last List.maxBy) >> part1

let Solve : (string seq -> int * int)  = both part1 part2
