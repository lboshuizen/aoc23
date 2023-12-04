module Day4

let winners w n = Set.intersect w n |> Seq.length
let numbers = splitOn ' ' >> Seq.filter ( (<>) "" ) >> Seq.map int >> Set

let parse s = let lr = s |> splitOn '|'
              let c = lr[0] |> splitOn ':'
              let g = c[0] |> parseRegex "Card\s+(\d+)" (fun a -> int a[0])
              g,((winners (numbers lr[1]) (numbers c[1])),1)                                      

let part1 = let game = function
                       | _,(0,_) -> 0
                       | _,(n,_) -> pown 2 (n-1)
                             
            Seq.map game >> Seq.sum

let addCopies m (k,v) = update m k (mapSnd ((+) v))

let winCopies cards = let f (m:Map<int,int*int>) cid =
                         let w,c = m[cid]
                         Seq.fold addCopies m [for nc in cid+1..cid+w do (nc,c)]
    
                      Seq.fold f cards [1..Map.count cards]

let part2 = Map >> winCopies >> Map.values >> Seq.sumBy snd

let Solve : (string seq -> int*int) = Seq.map parse >> both part1 part2
