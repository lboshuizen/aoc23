module Day09

let parse = Seq.map allInt

let rec history xs = let ds = xs |> Seq.pairwise |> Seq.map (fun (a,b) -> b-a)
                     match Seq.forall ((=)0) ds with
                     | true -> Seq.last xs
                     | false -> (Seq.last xs) + history ds

let part1 = Seq.sumBy history
let part2 =  Seq.sumBy (Seq.rev >> history)

let Solve : (string seq -> int*int) = parse >> both part1 part2 // 1641934234, 975 
