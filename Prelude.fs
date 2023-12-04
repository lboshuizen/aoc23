[<Microsoft.FSharp.Core.AutoOpen>]
module Prelude

open System.Text.RegularExpressions

let both f g x = (f x, g x)
let flip f a b = f b a

let foldl = Seq.fold

let isDigit c = System.Char.IsDigit c
let a2i (c:char) = int c - int '0'
let seq2i : (char seq -> int) = Seq.map a2i >> Seq.fold (fun a n -> a * 10 + n) 0

let splitOn (c:char) (s:string) = s.Split c
let splitOnAny (del:#seq<char>) (s:string) = Seq.toArray del |> s.Split

let parseRegex regex map s =  Regex.Match(s,regex) |> fun m -> m.Groups
                              |> Seq.skip 1 // ignore first group
                              |> Seq.map (fun a -> a.Value) |> Array.ofSeq |> map

type Grid<'a> = Map<int*int,'a>

// parse lines to grid in col-row order
let toGrid2d (xs:#seq<#seq<char>>) : ((int * int) * char) seq = 
    let ri row = Seq.mapi (fun col a -> ((col,row),a))
    xs |> Seq.mapi ri |> Seq.concat

let delete (m:Map<_,_>) = Seq.fold (flip Map.remove) m

// full scan around (x,y) omitting origin (0,0) 
let around (x,y) = Seq.map (fun (x',y') -> (x+x',y+y') ) [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1);]

let mapSnd f (a,b) = (a,f b)

let update (m:Map<_,_>) k f =  m |> Map.change k (fun x -> match x with
                                                           | Some v -> Some (f v)
                                                           | None -> None)
