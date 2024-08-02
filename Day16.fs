module Day16

let parse = toGrid2d >> Map

type Grid = Map<(int*int),char>

let next ((x,y),d) = match d with
                     | 'N' -> (x,y-1),d
                     | 'S' -> (x,y+1),d
                     | 'E' -> (x+1,y),d
                     | 'W' -> (x-1,y),d

let move (m:Grid) (p,d) = match Map.tryFind p m with
                           | None -> []
                           
                           | Some '.' -> [next (p,d)]
                           
                           | Some '|' -> match d with
                                         | 'N' | 'S' -> [next (p,d)]
                                         | 'E' | 'W' -> [next (p,'S'); next (p,'N')]
                           | Some '-' -> match d with
                                         | 'N' | 'S' -> [next (p,'E'); next (p,'W')]
                                         | 'E' | 'W' -> [next (p,d)]
                           | Some '\\' -> match d with
                                          | 'N' -> [next (p,'W')]
                                          | 'S' -> [next (p,'E')]
                                          | 'E' -> [next (p,'S')]
                                          | 'W' -> [next (p,'N')]
                           | Some '/' -> match d with
                                          | 'N' -> [next (p,'E')]
                                          | 'S' -> [next (p,'W')]
                                          | 'E' -> [next (p,'N')]
                                          | 'W' -> [next (p,'S')]
 
let energize m s =
    
    let rec go b t = let t' = List.map (move m) b |> List.concat |> List.filter (flip Set.contains t >> not)
                     match t' with
                     | [] -> Set.map fst t
                     | _ -> go t' (t + Set(t'))
        
    go [s] (Set([s])) |> Seq.filter (flip Map.containsKey m) |> Seq.length 
                       
let part1 (m:Grid) = energize m ((0,0),'E')

let part2 (m:Grid) = let mx = m |> Map.keys |> Seq.maxBy fst |> fst
                     let p = [for x in 0..mx -> (x,0)] @ [for x in 0..mx -> (x,mx)] @ [for x in 0..mx -> (0,x)] @ [for x in 0..mx -> (mx,x)]
                     let s = [for p' in p do for d in ['N';'S';'E';'W'] -> (p',d)] |> List.distinct |> List.toArray
                     
                     Array.Parallel.map (energize m) s |> Seq.max

let Solve (xs:string seq) = xs |> parse |> both part1 part2
