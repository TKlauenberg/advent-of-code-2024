#load "common.fsx"
open System

let readData (line: string) =
    line.Split(' ') |> Array.map int |> Array.toList

let computeReport (report) =
    report |> List.pairwise |> List.map (fun (a, b) -> a - b)

let checkIncreasingOrDecreasing (report) =
    let allIncreasing = report |> List.forall (fun x -> x < 0)
    let allDecreasing = report |> List.forall (fun x -> x > 0)
    allIncreasing || allDecreasing

let checkGradually (report) =
    report |> List.forall (fun x -> x >= -3 && x <= 3 && x <> 0)

let checkReport report =
    (checkIncreasingOrDecreasing report) && (checkGradually report)

let compute (data: string array) =
    data
    |> Array.map (readData >> computeReport >> checkReport)
    |> Array.filter id
    |> Array.length


let sample1 =
    """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""

let sample1Result = sample1.ByNewLine() |> compute
let part1 = Files[2] |> compute

let getAllVariants x =
    let length = Seq.length x

    seq {
        yield x
        yield List.tail x

        for i = 1 to (length - 1) do
            yield (x.[.. i - 1] @ x.[(i + 1) ..])
    }

let checkReport2 (report) =
    report |> getAllVariants |> Seq.map computeReport |> Seq.exists checkReport



let compute2 (data: string array) =
    data |> Array.map (readData >> checkReport2) |> Array.filter id |> Array.length


let sample2Result = sample1.ByNewLine() |> compute2
let part2 = Files[2] |> compute2
