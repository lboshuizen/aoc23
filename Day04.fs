module Day04

let parse s = let left,right = s |> splitOn '|' |> Array.map allInt |> pair
              let winners n w = Set.intersect (Set w) (Set n) |> Seq.length
              
              left[0],(winners left[1..] right,1)                                      

let part1 = let game = function
                       | _,(0,_) -> 0
                       | _,(n,_) -> pown 2 (n-1)
                             
            Seq.sumBy game

let addCopies m (k,v) = Map.update m k (mapSnd ((+) v))

let winCopies cards = let f (m:Map<int,int*int>) cid =
                          let w,c = m[cid]
                          Seq.fold addCopies m [for nc in cid+1..cid+w -> (nc,c)]
    
                      Seq.fold f cards [1..Map.count cards]

let part2 = Map >> winCopies >> Map.values >> Seq.sumBy snd

let Solve : (string seq -> int*int) = Seq.map parse >> both part1 part2
