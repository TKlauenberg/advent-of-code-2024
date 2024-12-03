#load "common.fsx"
open System
open System.Text.RegularExpressions

let sample1 =
    """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""
let getMatch (m: Match)= (int m.Groups.[1].Value, int m.Groups.[2].Value)
let multiply (a, b) = a * b
let compute line=
    Regex.Matches(line, "mul\\((\\d+),(\\d+)\\)")
    |> Seq.sumBy (getMatch >> multiply)

let sample1Result = sample1 |> compute

let result1 = System.IO.File.ReadAllText $"data/3.txt"|> compute

let getMatch2 (m: Match)= (m.Index,(int m.Groups.[1].Value, int m.Groups.[2].Value))


let getNextPosition dosOrDonts index =
    dosOrDonts
    |> Array.filter (fun x -> x < index)
    |> fun x -> if x.Length = 0 then 0 else x |> Array.max
let doFilter dos donts index =
    let lastDo = getNextPosition dos index
    let lastDont = getNextPosition donts index
    lastDo>=lastDont
let compute2 line =
    let muls = Regex.Matches(line, "mul\\((\\d+),(\\d+)\\)") |> Seq.map getMatch2
    let dos = Regex.Matches(line, "do\\(\\)") |> Seq.map (fun x -> x.Index) |> Seq.toArray
    let donts = Regex.Matches(line, "don't\\(\\)") |> Seq.map (fun x -> x.Index)|> Seq.toArray
    let filter = doFilter dos donts
    muls
    |> Seq.filter (fun x -> x |> fst |> filter)
    |> Seq.sumBy (snd>>multiply)

let sample2 ="xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
let sample2Result = sample2 |> compute2
let result2 = System.IO.File.ReadAllText $"data/3.txt"|> compute2