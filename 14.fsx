#load "common.fsx"
open System

let sampleWidthHeight = (11, 7)
let widthHeight = (101, 103)

let sample =
    """p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"""

type Point = { X: int; Y: int }
type Velocity = { X: int; Y: int }
type Robot = { Point: Point; Velocity: Velocity }

let remove x (s: string) = s.Replace(x, "")
let split (x: string) (s: string) = s.Split(x)

let replacer =
    [ "p="; "v=" ]
    |> List.map remove
    |> List.fold (fun acc (x: string -> string) -> acc >> x) id

let parseRobot line =
    let parts = line |> replacer |> split " " |> Array.collect (split ",")

    { Point = { X = int parts.[0]; Y = int parts.[1] }
      Velocity = { X = int parts.[2]; Y = int parts.[3] } }

let readData (data: string array) = data |> Array.map parseRobot

let mod' a b =
    let r = a % b
    if r < 0 then r + b else r
let simulateRobot (xBoundary, yBoundary) moves robot =
    let x = mod' (robot.Point.X + moves * robot.Velocity.X) xBoundary
    let y = mod' (robot.Point.Y + moves * robot.Velocity.Y) yBoundary
    { X = x; Y = y }

let getQuadrant (xBoundary, yBoundary) position =
    match position with
    | { X = x; Y = y } when x < xBoundary / 2 && y < yBoundary / 2 -> Some(1, 0, 0, 0)
    | { X = x; Y = y } when x > xBoundary / 2 && y < yBoundary / 2 -> Some(0, 1, 0, 0)
    | { X = x; Y = y } when x < xBoundary / 2 && y > yBoundary / 2 -> Some(0, 0, 1, 0)
    | { X = x; Y = y } when x > xBoundary / 2 && y > yBoundary / 2 -> Some(0, 0, 0, 1)
    | _ -> None

let computeQuadrants (xBoundary, yBoundary) positions =
    positions
    |> Array.choose (getQuadrant (xBoundary, yBoundary))
    |> Array.reduce (fun (a, b, c, d) (a', b', c', d') -> (a + a', b + b', c + c', d + d'))

let getQuadrantResult (a, b, c, d) = a * b * c * d

let sampleResult1 =
    sample.ByNewLine()
    |> readData
    |> Array.map (simulateRobot sampleWidthHeight 100)
    |> computeQuadrants sampleWidthHeight
    |> getQuadrantResult

let result1 =
    Files.[14]
    |> readData
    |> Array.map (simulateRobot widthHeight 100)
    |> computeQuadrants widthHeight
    |> getQuadrantResult

let printMap (xBoundary, yBoundary) positions =
    let map = positions |> Set.ofArray
    seq {
        for y in 0 .. yBoundary - 1 do
            for x in 0 .. xBoundary - 1 do
                yield if map.Contains({ X = x; Y = y }) then '#' else '.'
            yield '\n'
    }
    |> String.Concat

let robots =
    Files.[14]
    |> readData

let simulateRobots widthHeight robots moves =
    robots |> Array.map (simulateRobot widthHeight moves)
let simulations =
    [1..10000]
    |> List.map (simulateRobots widthHeight robots)
    |> List.iteri (fun i robots -> System.IO.File.WriteAllText($"data/14/{i}.txt", printMap widthHeight robots))


