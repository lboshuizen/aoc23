module Day9

let parse = Seq.map allInt

let history ft fr xs =
    let rec go xs a = let ds = xs |> Seq.pairwise |> Seq.map (fun (a,b) -> b-a)
                      match Seq.forall ((=)0) ds with
                      | true -> (ft xs) :: a
                      | false -> go ds ((ft xs) :: a)

    go xs [] |> Seq.reduce fr

let part1 = Seq.sumBy (history Seq.last (+))
let part2 = Seq.sumBy (history Seq.head (flip (-)))

let Solve (xs:string seq) = xs |> parse |> both part1 part2
