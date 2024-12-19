#load "common.fsx"
open System

let sample =
    """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"""

let remove x (s: string) = s.Replace(x, "")

let replacer =
    [ "Button A: X+"; " Y+"; "Button B: X+"; "Prize: X="; " Y=" ]
    |> List.map remove
    |> List.fold (fun acc (x: string -> string) -> acc >> x) id

type Button = { X: int64; Y: int64 }
type Prize = Button

type ClawMachine =
    { ButtonA: Button
      ButtonB: Button
      Price: Prize }

    static member fromData(data: string array) =
        let [| ax; ay |] =
            data.[0] |> replacer |> (fun x -> x.Split(",")) |> Array.map int64

        let [| bx; by |] =
            data.[1] |> replacer |> (fun x -> x.Split(",")) |> Array.map int64

        let [| px; py |] =
            data.[2] |> replacer |> (fun x -> x.Split(",")) |> Array.map int64

        { ButtonA = { X = ax; Y = ay }
          ButtonB = { X = bx; Y = by }
          Price = { X = px; Y = py } }

let readData (data: string array) =
    data |> Array.chunkBySize 4 |> Array.map ClawMachine.fromData


// first solution (brute force)
let allCombinationsOneDimension a b prize =
    seq { for i in 1L .. prize / (int64 a) -> i, prize - i * a }
    |> Seq.filter (fun (_, y) -> y % b = 0L)
    |> Seq.map (fun (x, y) -> x, y / b)

let allCombinations (buttonA: Button) (buttonB: Button) (prize: Prize) =
    let xCombinations = allCombinationsOneDimension buttonA.X buttonB.X prize.X
    let yCombinations = allCombinationsOneDimension buttonA.Y buttonB.Y prize.Y
    let yValues = yCombinations |> Set.ofSeq

    xCombinations |> Seq.filter (fun x -> yValues.Contains x)

let getCombination (buttonA: Button) (buttonB: Button) (prize: Prize) =
    let d = buttonA.X * buttonB.Y - buttonA.Y * buttonB.X
    let dx = prize.X * buttonB.Y - prize.Y * buttonB.X
    let dy = buttonA.X * prize.Y - buttonA.Y * prize.X

    match dx % d, dy % d with
    | 0L, 0L -> Some(dx / d, dy / d)
    | _, _ -> None

let getTokens (a, b) = a * 3L + b

let getCheapestPresses allCombinations =
    if Seq.isEmpty allCombinations then
        None
    else
        allCombinations |> Seq.minBy getTokens |> Some

let compute1 data =
    data
    |> Array.map (fun x -> allCombinations x.ButtonA x.ButtonB x.Price)
    |> Array.choose getCheapestPresses
    |> Array.sumBy getTokens



let sampleResult1 = sample.ByNewLine() |> readData |> compute1

let result1 = Files.[13] |> readData |> compute1

let convert clawMachine =
    { clawMachine with
        Price =
            { X = clawMachine.Price.X + 10000000000000L
              Y = clawMachine.Price.Y + 10000000000000L } }

let getCombinationClawMachine clawMachine =
    getCombination clawMachine.ButtonA clawMachine.ButtonB clawMachine.Price

let compute2 data =
    data |> Array.choose getCombinationClawMachine |> Array.sumBy getTokens


let sampleResult2 = sample.ByNewLine() |> readData |> Array.map convert |> compute2
let result2 = Files.[13] |> readData |> Array.map convert |> compute2
