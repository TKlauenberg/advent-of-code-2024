#load "common.fsx"
open System

let readData (line: string) =
    line.Split(' ')
    |> Array.filter (fun x -> x <> "")
    |> Array.map int
    |> (fun x -> (x.[0], x.[1]))

let sample1 =
    """3   4
4   3
2   5
1   3
3   9
3   3"""

let compute (data: string array) =
    data
    |> Seq.map readData
    |> Seq.toList
    |> List.unzip
    |> fun (a, b) -> (List.sort a, List.sort b)
    |> fun (a, b) -> List.zip a b
    |> List.sumBy ((fun (a, b) -> a - b) >> abs)

let sample1Result = sample1.ByNewLine() |> compute

let part1 = Files[1] |> compute

// part two
let sample2 =
    """3   4
4   3
2   5
1   3
3   9
3   3"""

let compute2 data =
    data
    |> Seq.map readData
    |> Seq.toList
    |> List.unzip
    |> fun (a, b) ->
        a
        |> List.map (fun x ->
            let count = b |> List.filter (fun y -> y = x) |> List.length
            x * count)
    |> List.sum

let sample2Result = sample2.ByNewLine() |> compute2

let part2 = Files[1] |> compute2
