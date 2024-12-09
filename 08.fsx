#load "common.fsx"
open System

let sample1 =
    """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""

type Position =
    { X: int
      Y: int }

    static member (+)(p1, p2) = { X = p1.X + p2.X; Y = p1.Y + p2.Y }
    static member (-)(p1, p2) = { X = p1.X - p2.X; Y = p1.Y - p2.Y }
    static member (*)(p, i) = { X = p.X * i; Y = p.Y * i }

type Antenna = { Position: Position; Frequency: char }

let readData (data: string array) =
    let antennas =
        data
        |> Array.toSeq
        |> Seq.mapi (fun y line ->
            line
            |> Seq.mapi (fun x c ->
                { Position = { X = x; Y = y }
                  Frequency = c })
            |> Seq.filter (fun antenna -> antenna.Frequency <> '.'))
        |> Seq.collect id

    let sizey = data.Length
    let sizex = data.[0].Length
    (antennas, sizey, sizex)

let getAntinodesBetweenAntennas (antenna1: Antenna) (antenna2: Antenna) =
    let distance = antenna1.Position - antenna2.Position
    [ antenna1.Position + distance; antenna2.Position - distance ] |> List.toSeq

let getAntinodeForAllAntennas getAntinodesBetweenAntennas (antennas: Antenna seq) =
    antennas
    |> Seq.collect (fun antenna1 ->
        antennas
        |> Seq.collect (fun antenna2 ->
            if antenna1 <> antenna2 then
                getAntinodesBetweenAntennas antenna1 antenna2
            else
                Seq.empty))

let isAntiNodeOnMap sizeX sizeY antinode =
    antinode.X >= 0 && antinode.X < sizeX && antinode.Y >= 0 && antinode.Y < sizeY

let compute (antennas, sizeX, sizeY) =
    antennas
    |> Seq.groupBy (fun antenna -> antenna.Frequency)
    |> Seq.collect (fun (_, antennas) -> getAntinodeForAllAntennas getAntinodesBetweenAntennas antennas)
    |> Seq.filter (isAntiNodeOnMap sizeX sizeY)
    |> Seq.distinct
    |> Seq.length

let sample1Result = sample1.ByNewLine() |> readData |> compute
let result1 = Files.[8] |> readData |> compute

// part 2
let getAntinodesBetweenAntennas2 isNodeOnMap (antenna1: Antenna) (antenna2: Antenna) =
    let distance = antenna1.Position - antenna2.Position
    // [ antenna1.Position + distance; antenna2.Position - distance ]
    [ Seq.initInfinite (fun i -> antenna1.Position + (distance * i))
      |> Seq.takeWhile isNodeOnMap
      Seq.initInfinite (fun i -> antenna2.Position - (distance * i))
      |> Seq.takeWhile isNodeOnMap ]
    |> Seq.concat

let compute2 (antennas, sizeX, sizeY) =
    antennas
    |> Seq.groupBy (fun antenna -> antenna.Frequency)
    |> Seq.collect (fun (_, antennas) ->
        getAntinodeForAllAntennas (getAntinodesBetweenAntennas2 (isAntiNodeOnMap sizeX sizeY)) antennas)
    |> Seq.distinct
    |> Seq.length

let sampleResult2 = sample1.ByNewLine() |> readData |> compute2
let result2 = Files.[8] |> readData |> compute2
