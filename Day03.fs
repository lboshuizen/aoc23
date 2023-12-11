module Day03

let parse = (toGrid2d >> Seq.filter (snd >> (<>) '.') >> Map)

let posIsDigit m xy = match Map.tryFind xy m with
                      | Some v when isDigit v -> true
                      | _ -> false
                      
let rec startOfNum m r c = match posIsDigit m (c,r) with
                            | true -> startOfNum m r (c-1)
                            | _ -> c+1
                      
let partNumber (g:Grid<char>) (c,r) = let gridSize = 142
                                      let numberFrom = Seq.map (fun p -> g[p]) >> String.fromChars >> int
                                      let sn = startOfNum g r c
                                      let pts = Seq.takeWhile (posIsDigit g) (seq {for x' in sn..gridSize -> (x',r)})
                                      r, numberFrom pts                           

let collect m = let touched m = around >> Seq.filter (posIsDigit m) >> Seq.map (partNumber m) >> Seq.distinct >> Seq.map snd
                Seq.map (touched m)
             
let part1  g = let symbols = Map.filter (fun _ v -> isDigit v |> not) >> Map.keys
               g |> symbols |> collect g |> Seq.concat |> Seq.sum

let part2 g = let gears = Map.filter (fun _ v -> v = '*') >> Map.keys
              let ratio = Seq.filter (Seq.length >> (=) 2) >> Seq.map (Seq.reduce (*))
              g |> gears |> collect g |> ratio |> Seq.sum

let Solve : (string seq -> int * int) = parse >> both part1 part2
