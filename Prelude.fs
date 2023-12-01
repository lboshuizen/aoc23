[<Microsoft.FSharp.Core.AutoOpen>]
module Prelude

let both f g x = (f x, g x)
 
let foldl = Seq.fold