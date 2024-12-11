#load "common.fsx"
open System

let sample =
    """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"""

let readData (data: string array) =
    data
    |> Array.mapi (fun y row -> row |> Seq.mapi (fun x c -> ((x, y), int c - int '0')))
    |> Seq.concat

let getNeighbours (x, y) =
    [ (x - 1, y); (x, y - 1); (x + 1, y); (x, y + 1) ]

let rec checkTrailhead map position value =
    let possibleNeighbours =
        position
        |> getNeighbours
        |> List.filter (fun p -> Map.containsKey p map && map.[p] = value + 1)

    if value = 8 then
        possibleNeighbours
    else
        possibleNeighbours
        |> List.map (fun p -> checkTrailhead map p (value + 1))
        |> List.concat

let computeTrailhead map (position,value) =
    checkTrailhead map position value
    |> Seq.distinct
    |> Seq.length
let compute (computeFn: Map<(int*int),int> -> ((int*int)*int) -> int) (data: ((int*int)*int) seq)  =
    let map = data |> Map.ofSeq
    data
    |> Seq.filter (fun (_, v) -> v = 0)
    |> Seq.sumBy (computeFn map)


let sampleResult1 = sample.ByNewLine() |> readData |> (compute computeTrailhead)
let result1 = Files[10] |> readData |> compute computeTrailhead

// part2
let computeTrailhead2 map (position,value) =
    checkTrailhead map position value
    |> Seq.length

let sampleResult2 = sample.ByNewLine() |> readData |> compute computeTrailhead2
let result2 = Files[10] |> readData |> compute computeTrailhead2