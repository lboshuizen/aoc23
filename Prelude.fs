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

let toGrid2d (xs:#seq<#seq<'a>>) : ((int * int) * 'a) list = 
    let ri y = Seq.mapi (fun x a -> ((x,y),a)) >> List.ofSeq
    xs |> Seq.mapi ri |> Seq.concat |> List.ofSeq

let delete (m:Map<_,_>) = Seq.fold (flip Map.remove) m

let around (x,y) = Seq.map (fun (x',y') -> (x+x',y+y') ) [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1);]

let mapSnd f (a,b) = (a,f b) 