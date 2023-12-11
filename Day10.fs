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

let expandPipes ((x,y),c) = [for x' in -1..1 do for y' in -1..1 -> (x',y')] |> Seq.map (fun (x',y') -> (3*x+x',3*y+y'),patterns[c][y'+1,x'+1])

let parse = toGrid2d >> both (Seq.filter (snd >> (<>) '.') >> both (Seq.find (snd >> (=) 'S') >> fst) (Seq.map (fun (xy,c) -> xy,translate xy c) >> Map))
                             Map

let next m p = match Map.lookup m p with
               | [p'] -> p'
               | xs -> xs |> Seq.filter (Map.hasKey m) |> Seq.head

let findLoop m s =
    let rec go m p a = match next m p with
                       | n when n=s -> p::a
                       | n -> go (Map.remove p m) n (p::a) 

    let toStart = m |> Map.filter (fun _ v -> List.length v > 1 && v |> List.contains s) |> Seq.head |> (fun kv -> kv.Key)
    let f1 = m[toStart] |> Seq.filter ((<>) s) |> Seq.head
    
    [s;toStart] @ go (Map.remove toStart m) f1 []

let part1 ((s,m),_) = findLoop m s |> Seq.length |> flip (/) 2 

let rec flood xy m = let isVoid p = Map.hasKey m p && (m[p] = '.' || m[p] = '#' )

                     [(1,0);(-1,0);(0,1);(0,-1)] |> Seq.map ((++)xy)
                     |> Seq.filter isVoid |> Seq.fold (fun m' p -> flood p m') (Map.remove xy m)

let ignoreJunk j v = v |> Map.keys |> Seq.filter (flip Set.contains j >> not) |> Seq.fold (fun m p -> Map.update m p (Const '.')) v

let part2 ((s,m),v) = v |> ignoreJunk (findLoop m s |> Set)
                        |> Map.entries |> Seq.collect expandPipes |> Map
                        |> flood (0,0)  
                        |> Map.entries |> Seq.filter (snd >> ((=)'#')) |> Seq.length |> flip (/) 9

let Solve : (string seq -> int*int) = parse >> both part1 part2 //(6613, 511)
