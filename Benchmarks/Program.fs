open System.Reflection
open BenchmarkDotNet.Running

BenchmarkRunner.Run(Assembly.GetExecutingAssembly()) |> ignore