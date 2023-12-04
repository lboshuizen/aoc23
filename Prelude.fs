[<Microsoft.FSharp.Core.AutoOpen>]
module Prelude

open System
open System.Collections.Generic
open System.Text.RegularExpressions

let both f g x = (f x, g x)
let flip f a b = f b a

let foldl = Seq.fold

let inline isDigit c = Char.IsDigit c
let inline a2i (c:char) = int c - int '0'

let splitOn (c:char) (s:string) = s.Split c
let splitOnAny (del:#seq<char>) (s:string) = Seq.toArray del |> s.Split

let parseRegex regex map s =  Regex.Match(s,regex) |> fun m -> m.Groups
                              |> Seq.skip 1 // ignore first group
                              |> Seq.map (fun a -> a.Value) |> Array.ofSeq |> map
let allInt = Regex(@"-?\d+").Matches >> Seq.map (fun m -> int m.Value) >> Array.ofSeq

type Grid<'a> = Map<int*int,'a>

// parse lines to grid in col-row order
let toGrid2d (xs:#seq<#seq<char>>) : ((int * int) * char) seq = 
    let ri row = Seq.mapi (fun col a -> ((col,row),a))
    xs |> Seq.mapi ri |> Seq.concat

// full scan around (x,y) omitting origin (0,0) 
let around (x,y) = Seq.map (fun (x',y') -> (x+x',y+y') ) [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1);]

let inline mapSnd f (a,b) = (a,f b)
let inline mapFst f (a,b) = (f a, b)

let curry f a b = f (a,b)
let uncurry f (a,b) = f a b

let tuple (a:IList<'a>) = (a[0],a[1])
let triple (a:'a array) = (a[0],a[1],a[2])

module String =
    let fromChars : (seq<char> -> string) = String.Concat

module Map =
    
    let private set f = function
                        | Some v -> Some (f v)
                        | None -> None
    
    let update (m:Map<_,_>) k f =  m |> Map.change k (set f)

    let delete (m:Map<_,_>) = Seq.fold (flip Map.remove) m
