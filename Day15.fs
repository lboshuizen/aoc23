module Day15

let parse = Seq.head >> splitOn ','

let hash = Seq.fold (fun n c -> ((n + (int c)) * 17) % 256) 0

type Op = | Add of int*string*int
          | Del of int*string

let decode (s:string) = if s.Contains "=" then
                          let a = splitOn '=' s
                          Add (hash a[0],a[0],int a[1])
                        else
                          let a = splitOn '-' s
                          Del (hash a[0],a[0])

let replace xs l v = match List.tryFindIndex (fun (lbl,_) -> lbl=l) xs with
                     | Some i -> let h,t = List.splitAt i xs
                                 h @ ((l,v) :: (List.tail t))
                     | None -> xs @ [(l,v)]

let toBox (bx:(string*int) list array) (s:string) =
    match decode s with
    | Add (b,lbl,fl) -> replace bx.[b] lbl fl |> Array.set bx b
    | Del (b,lbl) -> List.filter (fun (l,_) -> l <> lbl) bx.[b] |> Array.set bx b
    bx

let total = let fl = Seq.mapi(fun i (_,f) -> ((i+1) * f)) >> Seq.sum
            Seq.mapi(fun i b -> (i+1) * (fl b)) >> Seq.sum

let part1 = Seq.map hash >> Seq.sum

let part2 = Seq.fold toBox (Array.create 256 []) >> total

let Solve (xs:string seq) = xs |> parse |> both part1 part2
// 521434, 248279