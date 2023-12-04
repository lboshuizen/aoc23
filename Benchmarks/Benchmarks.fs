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
    member _.Part1() = Day1.part1 data
    
    //[<Benchmark>]
    member _.Part2() = Day1.part2 data
    
[<MemoryDiagnoser>]
type BDay2() =

    let data = readInput 2 |> Seq.map Day2.parse
        
    //[<Benchmark>]
    member _.Part1() = Day2.part1 data
    
    //[<Benchmark>]
    member _.Part2() = Day2.part2 data
    
    
[<MemoryDiagnoser>]
type BDay3() =

    let data = readInput 3 |> Day3.parse
        
    [<Benchmark>]
    member _.Part1() = Day3.part1 data
    
    [<Benchmark>]
    member _.Part2() = Day3.part2 data


[<MemoryDiagnoser>]
type BDay4() =

    let raw = readInput 4
    let data = Seq.map Day4.parse raw 
        
    [<Benchmark>]
    member _.Parse1() = Day4.parse raw[0]

    [<Benchmark>]
    member _.ParseAll() = List.map Day4.parse raw

            
    [<Benchmark>]
    member _.Part1() = Day4.part1 data
    
    [<Benchmark>]
    member _.Part2() = Day4.part2 data
    