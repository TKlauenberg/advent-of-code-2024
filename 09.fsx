#load "common.fsx"
open System

let sample = "2333133121414131402"

type SpaceBlock =
    { Value: int
      Amount: int
      FreeSpace: int }

let readBlock value x =
    match x with
    | [| x; y |] ->
        { Value = value
          Amount = (x - '0') |> int
          FreeSpace = (y - '0') |> int }
    | [| x |] ->
        { Value = value
          Amount = (x - '0') |> int
          FreeSpace = 0 }
    | _ -> failwith "Invalid input"

let readData data =
    data |> Seq.chunkBySize 2 |> Seq.mapi readBlock |> Seq.toList

let getValuesFromBlock block = List.replicate block.Amount block.Value

let rec getReversedValues data amount =
    match amount with
    | 0 -> [], data
    | _ ->
        let next = data |> List.head

        match next.Amount with
        | x when x > amount ->
            let newAmount = x - amount
            (next |> getValuesFromBlock |> List.take amount), { next with Amount = newAmount } :: (data |> List.tail)
        | _ ->
            let values = next |> getValuesFromBlock
            let newAmount = amount - next.Amount
            let newValues, newData = getReversedValues (data |> List.tail) newAmount
            values @ newValues, newData

let rec compact data =
    let dataReversed = data |> List.tail |> List.rev

    seq {
        yield! data |> List.head |> getValuesFromBlock

        let (values, remaining) =
            data |> List.head |> (fun x -> x.FreeSpace) |> getReversedValues dataReversed

        yield! values
        yield! remaining |> List.rev |> compact
    }

let calculateChecksum data =
    data
    |> Seq.mapi (fun i value -> (i, value))
    |> Seq.sumBy ((fun (i, value) -> i * value) >> uint64)

let compute data =
    let compactedLength = data |> List.sumBy (fun x -> x.Amount)
    compact data |> Seq.take compactedLength |> calculateChecksum


let sampleResult1 = sample |> readData |> compute
let result1 = Files[9] |> Array.head |> readData |> compute

// part 2
let moveBlock block (inserted, acc) x =
    if inserted then
        if x.Value = block.Value then
            inserted, {x with Amount=0; Value= -1;FreeSpace=x.FreeSpace+x.Amount}::acc
        else
            inserted, x :: acc
    else if x.FreeSpace >= block.Amount then
        true,
        { block with FreeSpace = x.FreeSpace - block.Amount }
        :: { x with
               FreeSpace = 0 }
        :: acc
    else if x.Value=block.Value then
        true, x::acc
    else
        false, x :: acc

let moveData data value =
    data
    |> List.fold (fun (inserted, acc) x -> moveBlock value (inserted, acc) x) (false, [])
    |> snd
    |> List.rev

let compact2 data =
    data |> List.rev |> List.fold (fun acc x -> moveData acc x) data

let rec compactedSpaceBlockToValues visitedValues data =
    seq {
        let current = data |> List.head

        if (visitedValues |> Set.contains current.Value |> not) then
            yield! List.replicate current.Amount (Some current.Value)

        yield! List.replicate current.FreeSpace None

        if not (data |> List.tail |> List.isEmpty) then
            yield!
                data
                |> List.tail
                |> compactedSpaceBlockToValues (visitedValues |> Set.add current.Value)
    }

let calculateChecksum2 data =
    data
    |> Seq.mapi (fun i value -> (i, value))
    |> Seq.filter (snd >> Option.isSome)
    |> Seq.sumBy ((fun (i, value) -> i * value.Value) >> uint64)

let compute2 data =
    data |> compact2 |> compactedSpaceBlockToValues Set.empty |> calculateChecksum2

let simpleData data = data |> Seq.map (fun x -> if x = None then '.' else '0' + (char x.Value)) |> Seq.toArray
let sampleResult2 = sample |> readData |> compute2 
let result2 = Files[9] |> Array.head |> readData |> compute2