module Day10

let translate (x,y) = function
                      | '|' -> [(x,y+1);(x,y-1)]
                      | '-' -> [(x-1,y);((x+1),y)]
                      | 'L' -> [(x+1,y);(x,y-1)]
                      | 'J' -> [(x,y-1);(x-1,y)]
                      | '7' -> [(x-1,y);(x,y+1)]
                      | 'F' -> [(x+1,y);(x,y+1)]
                      | 'S' -> [(x,y)]
                      | c -> failwith "unexpected %A{c}"

let patterns = [
                '-',array2D [|[|'.'; '.'; '.'|]; [|'-'; '-'; '-'|]; [|'.'; '.'; '.'|]|]
                '|',array2D [|[|'.'; '|'; '.'|]; [|'.'; '|'; '.'|]; [|'.'; '|'; '.'|]|]
                'L',array2D [|[|'.'; '|'; '.'|]; [|'.'; 'L'; '-'|]; [|'.'; '.'; '.'|]|]
                'J',array2D [|[|'.'; '|'; '.'|]; [|'-'; 'J'; '.'|]; [|'.'; '.'; '.'|]|]
                '7',array2D [|[|'.'; '.'; '.'|]; [|'-'; '7'; '.'|]; [|'.'; '|'; '.'|]|]
                'F',array2D [|[|'.'; '.'; '.'|]; [|'.'; 'F'; '-'|]; [|'.'; '|'; '.'|]|]
                'S',array2D [|[|'S'; 'S'; 'S'|]; [|'S'; 'S'; 'S'|]; [|'S'; 'S'; 'S'|]|]
                '.',array2D [|[|'#'; '#'; '#'|]; [|'#'; '#'; '#'|]; [|'#'; '#'; '#'|]|]
               ] |> Map

let parse = toGrid2d >> both (Seq.filter (snd >> (<>) '.') >> both (Seq.find (snd >> (=) 'S') >> fst) (Seq.map (fun (xy,c) -> xy,translate xy c) >> Map))
                             Map

let lookup m p = Map.tryFind p m
let canVisit m p = match lookup m p with
                   | None -> false
                   | _ -> true

let move m p = match lookup m p with
               | Some [p'] -> p'
               | Some xs -> xs |> Seq.filter (canVisit m) |> Seq.head
               | None -> failwith $"oops %A{p}"

let walk m s =
    let rec go m p a = match move m p with
                       | n when n=s -> p::a
                       | n -> go (Map.remove p m) n (p::a) 

    let toStart = m |> Map.filter (fun _ v -> List.length v > 1 && v |> List.contains s) |> Seq.head |> (fun kv -> kv.Key)
    let f1 = m[toStart] |> Seq.filter ((<>) s) |> Seq.head
    
    [s;toStart] @ go (Map.remove toStart m) f1 []

let part1 ((s,m),_) = walk m s |> Seq.length |> fun n -> n/2 

let rec flood (x,y) m = let isVoid p = canVisit m p && (m[p] = '.' || m[p] = '#' )
                        [(1,0);(-1,0);(0,1);(0,-1)] |> Seq.map (fun (x',y') -> (x+x'), (y+y'))
                        |> Seq.filter isVoid |> Seq.fold (fun m' p -> flood p m') (Map.update m (x,y) (Const '@'))

let ignoreJunk j v = v |> Map.keys |> Seq.filter (flip Set.contains j >> not) |> Seq.fold (fun m p -> Map.update m p (Const '.')) v

let expandPipes ((x,y),c) = [for x' in -1..1 do for y' in -1..1 -> (x',y')] |> Seq.map (fun (x',y') -> (3*x+x',3*y+y'),patterns[c][y'+1,x'+1])

let part2 ((s,m),v) = v |> ignoreJunk (walk m s |> Set)
                        |> Map.entries |> Seq.collect expandPipes |> Map
                        |> flood (0,0)  
                        |> Map.entries |> Seq.filter (snd >> ((=)'#')) |> Seq.length |> flip (/) 9

let Solve : (string seq -> int*int) = parse >> both part1 part2
