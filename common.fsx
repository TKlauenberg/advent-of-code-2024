[<AutoOpen>]
module Common

open System
open type System.Environment

type String with

    member this.ByNewLine() =
        this.Split([| NewLine; "\n" |], StringSplitOptions.RemoveEmptyEntries)

    /// This reusable function takes a multiline string and groups up based on whenever an empty line occurs.
    member this.GroupByEmptyLine() =
        this.Split([| NewLine + NewLine; "\n" + "\n" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun group -> group.ByNewLine() |> Array.toList)
        |> Array.toList

type Files() =
    member _.Item
        with get file = System.IO.File.ReadAllLines $"data/{file}.txt"

/// Provides access to data files using an indexer e.g. Files.[1] gets the path to the Day One data file.
let Files = Files()

module List =
    let partitionMap partitioner =
        let rec loop (acc1, acc2) =
            function
            | [] -> List.rev acc1, List.rev acc2
            | x :: xs ->
                match partitioner x with
                | Choice1Of2 y -> loop (y :: acc1, acc2) xs
                | Choice2Of2 y -> loop (acc1, y :: acc2) xs

        loop ([], [])

let (|Split|) (on: char) (s: string) =
    s.Split(on, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.toList

let (|Int|_|) (s: string) =
    match Int32.TryParse s with
    | true, n -> Some(Int n)
    | false, _ -> None

let (|UInt64|_|) (s: string) =
    match UInt64.TryParse s with
    | true, n -> Some(UInt64 n)
    | false, _ -> None

let (|Chars|) (s: string) = s.ToCharArray() |> Array.toList