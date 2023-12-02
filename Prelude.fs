[<Microsoft.FSharp.Core.AutoOpen>]
module Prelude

open System.Text.RegularExpressions

let both f g x = (f x, g x)
 
let foldl = Seq.fold

let isDigit c = System.Char.IsDigit c
let atoi (c:char) = int c - int '0'

let splitOn (c:char) (s:string) = s.Split c
let parseRegex regex map s =  Regex.Match(s,regex) |> fun m -> m.Groups
                              |> Seq.skip 1 // ignore first group
                              |> Seq.map (fun a -> a.Value) |> Array.ofSeq |> map
