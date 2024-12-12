#load "common.fsx"
open System

let sample = "125 17"

let parse (input: string) =
    input.Split [| ' ' |] |> Array.map uint64 |> Array.toList

let getHalfes input =
    let length = Seq.length input
    let half = length / 2

    [ input |> Seq.take half |> Seq.toArray |> String
      input |> Seq.skip half |> Seq.toArray |> String ]

let blinkStone stone =
    match stone with
    | 0UL -> [ 1UL ]
    | x when (x.ToString().Length) % 2 = 0 -> x.ToString() |> getHalfes |> List.map uint64
    | _ -> [ stone * 2024UL ]

let blinkOnce stones =
    stones |> List.map blinkStone |> List.concat

let rec blink n stones =
    match n with
    | 0 -> stones
    | _ -> blink (n - 1) (blinkOnce stones)

let sampleResult1 = sample |> parse |> blink 25 |> List.length
let result1 = Files[11] |> Array.head |> parse |> blink 25 |> List.length

// Part 2
// for this I looked at https://github.com/richardjharding/aoc2024/blob/main/Day11/Day11.dib
let createStarter data=
    data|> List.fold (fun acc x -> acc |> Map.add x 1UL) Map.empty

// from @mafinar.bsky.social on bluesky
let digits =
    function
    |0UL -> 1
    |x->1+(Math.Log10 (float x) |> int)
let getHalfes2 input halfLength=
    let left = input / (uint64 (Math.Pow(10.0, float halfLength)))
    let right = input % (uint64 (Math.Pow(10.0, float halfLength)))
    [left; right]

let add stone count map=
    match Map.tryFind stone map with
    | Some x -> Map.add stone (x + count) map
    | None -> Map.add stone count map
let blinkStone2 stone count  acc=
    match stone with
    | 0UL -> acc |> Map.remove 0UL |> Map.add 1UL count
    | x when digits x % 2 = 0 -> getHalfes2 x (digits x / 2) |> List.fold (fun acc x -> add x count acc) acc
    | _ -> add (stone * 2024UL) count acc

let blinkOnce2 stones =
    stones |> Map.fold (fun acc stone count -> blinkStone2 stone count acc) Map.empty
let rec blink2 n stones =
    match n with
    | 0 -> stones
    | _ -> blink2 (n - 1) (blinkOnce2 stones)


let result2 = Files[11] |> Array.head |> parse |> createStarter |> blink2 75 |> Map.fold (fun acc stone count -> acc + count) 0UL