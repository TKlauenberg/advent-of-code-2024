#load "common.fsx"
open System

let sample1 = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""

let readData (lines:string array)=
    lines
    |> Array.map (fun line -> line |> Seq.map (fun value -> value )|> Seq.toArray)

// we need to find the characters XMAS so we ge tall possible values from the map
let getAllPossibleValues map (x,y) =
    let yLength = Array.length map
    let xLength = Array.length map.[0]
    seq {
        if x+3<xLength then yield [for i in 0..3 -> map.[y].[x+i]]
        if x-3>=0 then yield [for i in 0..3 -> map.[y].[x-i]]
        if y+3<yLength then yield [for i in 0..3 -> map.[y+i].[x]]
        if y-3>=0 then yield [for i in 0..3 -> map.[y-i].[x]]
        if x+3<xLength && y+3<yLength then yield [for i in 0..3 -> map.[y+i].[x+i]]
        if x-3>=0 && y-3>=0 then yield [for i in 0..3 -> map.[y-i].[x-i]]
        if x+3<xLength && y-3>=0 then yield [for i in 0..3 -> map.[y-i].[x+i]]
        if x-3>=0 && y+3<yLength then yield [for i in 0..3 -> map.[y+i].[x-i]]
    }
    |> Seq.map (fun x -> x |> List.toArray |> String)

let getAllPossibleValuesWithPosition map (x,y) =
    let yLength = Array.length map
    let xLength = Array.length map.[0]
    seq {
        if x+3<xLength then yield ([for i in 0..3 -> map.[y].[x+i]],x,y,"right")
        if x-3>=0 then yield ([for i in 0..3 -> map.[y].[x-i]],x,y,"left")
        if y+3<yLength then yield ([for i in 0..3 -> map.[y+i].[x]],x,y,"down")
        if y-3>=0 then yield ([for i in 0..3 -> map.[y-i].[x]],x,y,"up")
        if x+3<xLength && y+3<yLength then yield ([for i in 0..3 -> map.[y+i].[x+i]],x,y,"right-down")
        if x-3>=0 && y-3>=0 then yield ([for i in 0..3 -> map.[y-i].[x-i]],x,y,"left-up")
        if x+3<xLength && y-3>=0 then yield ([for i in 0..3 -> map.[y-i].[x+i]],x,y,"right-up")
        if x-3>=0 && y+3<yLength then yield ([for i in 0..3 -> map.[y+i].[x-i]],x,y,"left-down")
    }
    |> Seq.map (fun (word,x,y,direction) -> (word |> List.toArray |> String),x,y,direction)


// let getNeighbours map (x,y) =
//     let yLength = Array.length map
//     let xLength = Array.length map.[0]
//     [
//         (x-1,y-1);(x,y-1);(x+1,y-1);
//         (x-1,y);(x+1,y);
//         (x-1,y+1);(x,y+1);(x+1,y+1)
//     ]
//     |> List.filter (fun (x,y) -> x>=0 && x<xLength && y>=0 && y<yLength)
let compute map=
    let yLength = Array.length map
    let xLength = Array.length map.[0]
    seq {
        for y in 0..yLength-1 do
            for x in 0..xLength-1 do
                if map.[y].[x]='X' then
                    yield! getAllPossibleValuesWithPosition map (x,y)
    }
    |> Seq.filter (fun (word,_,_,_) -> word="XMAS")
    |> Seq.length

let sample1Result = sample1.ByNewLine() |> readData |> compute
let result1 = Files.[4] |> readData |> compute

let getAllPossibleValuesWithPosition2 map (x,y) =
    let yLength = Array.length map
    let xLength = Array.length map.[0]
    seq {
        if x+2<xLength && y+2<yLength then yield ([for i in 0..2 -> map.[y+i].[x+i]],x+1,y+1,"right-down")
        if x-2>=0 && y-2>=0 then yield ([for i in 0..2 -> map.[y-i].[x-i]],x-1,y-1,"left-up")
        if x+2<xLength && y-2>=0 then yield ([for i in 0..2 -> map.[y-i].[x+i]],x+1,y-1,"right-up")
        if x-2>=0 && y+2<yLength then yield ([for i in 0..2 -> map.[y+i].[x-i]],x-1,y+1,"left-down")
    }
    |> Seq.map (fun (word,x,y,direction) -> (word |> List.toArray |> String),x,y,direction)
let compute2 map=
    let yLength = Array.length map
    let xLength = Array.length map.[0]
    seq {
        for y in 0..yLength-1 do
            for x in 0..xLength-1 do
                if map.[y].[x]='M' then
                    yield! getAllPossibleValuesWithPosition2 map (x,y)
    }
    |> Seq.filter (fun (word,_,_,_) -> word="MAS")
    |> Seq.countBy (fun (_,x,y,_)->(x,y))
    |> Seq.filter (fun (_,count)->count=2)
    |> Seq.length

let sample2Result = sample1.ByNewLine() |> readData |> compute2
let result2 = Files.[4] |> readData |> compute2