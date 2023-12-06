module Day6

open System

let abc (a,b,c) = let d = Math.Sqrt(b*b-4.0*a*c)
                  (-b-d)/(2.0*a),(-b+d)/(2.0*a)

let options (t,d) = let l,u = abc (1.0,float -t,float d)
                    let s,e = Math.Floor(l+1.0),Math.Ceiling(u-1.0)
                    int64 (e-s) + 1L

let part1 = (Seq.map allInt64 >> Array.ofSeq) >> fun a -> Seq.zip a[0] a[1]
            >> Seq.map options >> Seq.reduce (*)

let part2 = Seq.map (Seq.filter isDigit >> String.fromChars >> int64) >> Seq.toArray >> fun a -> options (a[0],a[1])

let Solve : (string seq -> int64 * int64) = both part1 part2
