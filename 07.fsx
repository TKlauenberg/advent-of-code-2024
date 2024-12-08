#load "common.fsx"
open System

let sample1 =
    """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

let readLine (line: string) =
    let parts = line.Split ':'

    (uint64 parts.[0],
     parts.[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
     |> Array.toSeq
     |> Seq.map uint64)

let readData (lines: string array) = lines |> Array.map readLine

let rec getPossibleValuesPart1 start (rest: uint64 seq) =
    seq {
        if Seq.isEmpty rest then
            yield start
        else
            let next = rest |> Seq.head
            yield! getPossibleValuesPart1 (start + next) (rest |> Seq.tail)
            yield! getPossibleValuesPart1 (start * next) (rest |> Seq.tail)
    }

let computeLine getPossibleValues (result, values) =
    let start = values |> Seq.head
    let rest = values |> Seq.tail
    getPossibleValues start rest |> Seq.exists ((=) result)

let compute getPossibleValues (data: (uint64 * uint64 seq) array) =
    data |> Array.filter (computeLine getPossibleValues) |> Array.sumBy fst

let resultSample1 =
    sample1.ByNewLine() |> readData |> (compute getPossibleValuesPart1)

let result1 = Files[7] |> readData |> (compute getPossibleValuesPart1)

// part 2
let rec getPossibleValuesPart2 start (rest: uint64 seq) =
    seq {
        if Seq.isEmpty rest then
            yield start
        else
            let next = rest |> Seq.head
            yield! getPossibleValuesPart2 (start + next) (rest |> Seq.tail)
            yield! getPossibleValuesPart2 (start * next) (rest |> Seq.tail)
            let concatValue = ((start |> string) + (next |> string)) |> uint64
            yield! getPossibleValuesPart2 (concatValue) (rest |> Seq.tail)
    }
let compute2 = compute getPossibleValuesPart2

let resultSample2= sample1.ByNewLine() |> readData |> compute2
let result2 = Files[7] |> readData |> compute2