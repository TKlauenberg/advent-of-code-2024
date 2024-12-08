#load "common.fsx"
open System

let sample1 =
    """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""


type CellState =
    | Empty
    | Obstruction
    | Start

type Position = (int * int)

type PatrolMap = Map<Position, CellState>

let readCellState value =
    match value with
    | '.' -> Empty
    | '^' -> Start
    | _ -> Obstruction


let readData (lines: string array) =
    let sizeX = lines |> Array.length
    let sizeY = lines.[0] |> String.length

    let map =
        lines
        |> Array.map (fun line -> line |> Seq.map (fun cell -> (readCellState cell)))
        |> Array.toSeq

    (sizeX, sizeY, map)

let createMapSeq data =
    data
    |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x cell -> ((x, y), cell)))
    |> Seq.concat

type Direction =
    | Up
    | Down
    | Left
    | Right

let move (position: Position) direction =
    match direction with
    | Up -> (fst position, snd position - 1)
    | Down -> (fst position, snd position + 1)
    | Left -> (fst position - 1, snd position)
    | Right -> (fst position + 1, snd position)

let turnRight direction =
    match direction with
    | Up -> Right
    | Down -> Left
    | Left -> Up
    | Right -> Down

let isObstruction map position =
    match Map.tryFind position map with
    | Some Obstruction -> true
    | _ -> false

let isOutOfMap (xSize, ySize) (x, y) =
    x < 0 || x >= xSize || y < 0 || y >= ySize

let getNextPosition (sizeX, sizeY, map) (position, direction) =
    let nextPosition = move position direction

    if isOutOfMap (sizeX, sizeY) nextPosition then
        None
    else if isObstruction map nextPosition then
        Some(position, turnRight direction)
    else
        Some(nextPosition, direction)

let rec getRoute (sizeX, sizeY, map) position direction =
    seq {
        let nextPosition = move position direction

        if isOutOfMap (sizeX, sizeY) nextPosition then
            yield position, direction
        else if isObstruction map nextPosition then
            yield! getRoute (sizeX, sizeY, map) position (turnRight direction)
        else
            yield position, direction
            yield! getRoute (sizeX, sizeY, map) nextPosition direction
    }

let compute (sizeX, sizeY, data) =
    let mapSeq = createMapSeq data
    let map = mapSeq |> Map.ofSeq
    let startPosition = mapSeq |> Seq.find (fun (_, state) -> state = Start) |> fst

    getRoute (sizeX, sizeY, map) startPosition Up
    |> Seq.map fst
    |> Seq.distinct
    |> Seq.length

let sample1Result = sample1.ByNewLine() |> readData |> compute

let result1 = Files.[6] |> readData |> compute

// part 2

let rec checkIfLoop (sizeX, sizeY, map) visited (position, direction) =
    let newPositionAndDirection = getNextPosition (sizeX, sizeY, map) (position, direction)

    match newPositionAndDirection with
    | None -> false
    | Some(newPosition, newDirection) when visited |> Set.contains (newPosition, newDirection) -> true
    | Some(newPosition, newDirection) ->
        checkIfLoop (sizeX, sizeY, map) (Set.add (newPosition, newDirection) visited) (newPosition, newDirection)

let setObstacle (sizeX, sizeY, map) position =
    map |> Map.add position Obstruction

let checkLoopFromRoute (sizeX, sizeY, map) start obstacle =
    let visited = Set.empty

    let mapWithNewObstacle = setObstacle (sizeX, sizeY, map) (obstacle|>fst)
    if checkIfLoop (sizeX, sizeY, mapWithNewObstacle) visited (start)
    then
        Some (obstacle |> fst)
    else
        None


let compute2 (sizeX, sizeY, data) =
    let mapSeq = createMapSeq data
    let map = mapSeq |> Map.ofSeq
    let startPosition = mapSeq |> Seq.find (fun (_, state) -> state = Start) |> fst
    let checkLoop = checkLoopFromRoute (sizeX, sizeY, map) (startPosition, Up)

    getRoute (sizeX, sizeY, map) startPosition Up
    |> Seq.choose checkLoop
    |> Seq.distinct
    |> Seq.length

let sample1Result2 = sample1.ByNewLine() |> readData |> compute2

let result2 = Files.[6] |> readData |> compute2
