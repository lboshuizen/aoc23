[<Microsoft.FSharp.Core.AutoOpen>]
module Prelude

open System
open System.Collections.Generic
open System.Text.RegularExpressions

let both f g x = (f x, g x)
let flip f a b = f b a

let swap (a,b) = (b,a)

let inline isDigit c = Char.IsDigit c
let inline a2i (c:char) = int c - int '0'

let foldl = Seq.fold
let foldr f = flip (List.foldBack f) // Who(??) decided to give foldBack that crazy signature

let splitOn (c:char) (s:string) = s.Split c
let splitOnAny (del:#seq<char>) (s:string) = Seq.toArray del |> s.Split
let splitWhen (pred:'a->bool) =
    let splitter c (r,f) = match pred c with
                           | true -> ([],r::f)
                           | _ -> (c::r,f)
                           
    foldr splitter ([],[]) >> fun (r,f) -> r::f
    
let splitOnEmpty = splitWhen ((=)"")

let parseRegex regex map s =  Regex.Match(s,regex) |> fun m -> m.Groups
                              |> Seq.skip 1 // ignore first group
                              |> Seq.map (fun a -> a.Value) |> Array.ofSeq |> map
let allInt = Regex(@"-?\d+").Matches >> Seq.map (fun m -> int m.Value) >> Array.ofSeq
let allInt64 = Regex(@"-?\d+").Matches >> Seq.map (fun m -> int64 m.Value) >> Array.ofSeq

type Grid<'a> = Map<int*int,'a>

let compare = function
              | a,b when a > b -> 1
              | a,b when a < b  -> -1
              | _ -> 0

let Const x = fun _ -> x

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

let rec gcd (a:int64) (b:int64) = if b = 0 then abs a else gcd (abs b) ((abs a) % abs b)
let lcm a b = (a*b) / (gcd a b)

let tap a = printfn $"%A{a}"
            a

let biMap f (a,b) = f a,f b

let rec iterate f x =
        seq {
            yield x
            yield! iterate f (f x)
        }

let infinite (a:'a) = iterate id a

let until p f i = Seq.scan f i >> Seq.takeWhile (p >> not)

module String =
    let fromChars : (seq<char> -> string) = String.Concat
    let replace (xs:string seq) (r:string) (s:string) = Seq.fold (fun (s':string) c -> s'.Replace(c,r)) s xs

let manhattan (x,y) (x',y') = abs (x-x') + abs (y-y')

let combinations xs =
    let rec go xs = match xs with
                    | [] -> []
                    | x::xs -> List.map (fun b -> (x,b)) xs @ go xs
    go xs        

module Map =
    
    let private set f = function
                        | Some v -> Some (f v)
                        | None -> None
    
    let update (m:Map<_,_>) k f =  m |> Map.change k (set f)

    let delete (m:Map<_,_>) = Seq.fold (flip Map.remove) m
    
    let lookup (m:Map<_,_>) k = m[k]

    let entries (m:Map<_,_>) = m |> Seq.map (fun kv -> kv.Key,kv.Value)
    
    let draw size (m:Map<int*int,char>) =                                    
         entries m |> Seq.sortBy (fst >> swap)
         |> Seq.chunkBySize size
         |> Seq.map (Seq.map snd >> String.fromChars)
         |> Seq.iter (printfn "%s")
         m
