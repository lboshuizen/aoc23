module Days
open System.IO

open BenchmarkDotNet.Attributes

let readInput (d:int)  =
    let p = Path.Combine(__SOURCE_DIRECTORY__,"..","inputs",$"day{d}.txt")
    printfn "%s" p
    File.ReadLines(p) |> List.ofSeq

[<MemoryDiagnoser>]
type BDay1() =

    let data = readInput 1
        
    //[<Benchmark>]
    member _.Part1() = Day01.part1 data
    
    //[<Benchmark>]
    member _.Part2() = Day01.part2 data
    
[<MemoryDiagnoser>]
type BDay2() =

    let data = readInput 2 |> Seq.map Day02.parse
        
    //[<Benchmark>]
    member _.Part1() = Day02.part1 data
    
    //[<Benchmark>]
    member _.Part2() = Day02.part2 data
    
    
[<MemoryDiagnoser>]
type BDay3() =

    let data = readInput 3 |> Day03.parse
        
    //[<Benchmark>]
    member _.Part1() = Day03.part1 data
    
    //[<Benchmark>]
    member _.Part2() = Day03.part2 data


[<MemoryDiagnoser>]
type BDay4() =

    let raw = readInput 4
    let data = Seq.map Day04.parse raw 
        
    //[<Benchmark>]
    member _.Parse1() = Day04.parse raw[0]

    //[<Benchmark>]
    member _.ParseAll() = List.map Day04.parse raw

            
    //[<Benchmark>]
    member _.Part1() = Day04.part1 data
    
    //[<Benchmark>]
    member _.Part2() = Day04.part2 data
    
 [<MemoryDiagnoser>]
type BDay8() =
    let data = readInput 8 |> Day08.parse 
            
    [<Benchmark>]
    member _.Part1() = Day08.part1 data
    
    [<Benchmark>]
    member _.Part2() = Day08.part2 data
