[<Microsoft.FSharp.Core.AutoOpen>]
module Prelude

let both f g x = (f x, g x)
 
let foldl = Seq.fold

let isDigit c = System.Char.IsDigit c
let atoi (c:char) = int c - int '0'
