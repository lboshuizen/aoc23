module Day3

let parse = (toGrid2d >> Seq.filter (snd >> (<>) '.') >> Map)

let posIsDigit m xy = match Map.tryFind xy m with
                      | Some v when isDigit v -> true
                      | _ -> false
                      
let partNumber (g:Grid<char>) (c,r) = let gridSize = 142
                                      let numberFrom = Seq.map (fun p -> g[p]) >> seq2i
                                      let pts = (List.takeWhile (posIsDigit g) ([for x' in 0..c-1 -> (x',r)] |> List.rev) |> List.rev)
                                                @ [(c,r)]
                                                @ List.takeWhile (posIsDigit g) [for x' in c+1..gridSize -> (x',r)]
                                      r,numberFrom pts                           

let collect m = let touched m = around >> Seq.filter (posIsDigit m) >> Seq.map (partNumber m) >> Seq.distinct >> Seq.map snd
                Seq.map (touched m)
             
let part1  g = let symbols = Map.filter (fun _ v -> isDigit v |> not) >> Map.keys
               g |> symbols |> collect g |> Seq.concat |> Seq.sum

let part2 g = let gears = Map.filter (fun _ v -> v = '*') >> Map.keys
              let ratio = Seq.filter (Seq.length >> (=) 2) >> Seq.map (Seq.reduce (*))
              g |> gears |> collect g |> ratio |> Seq.sum

let Solve : (string seq -> int * int) = parse >> both part1 part2
