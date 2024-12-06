#load "common.fsx"
open System

let sample1 =
    """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""

let readRule (rule: string) =
    let parts = rule.Split('|')
    (int parts.[0], int parts.[1])

let readPage (page: string) = page.Split(',') |> Array.map int

let readData (lines: string array) =
    let splitIndex = lines |> Array.tryFindIndex (fun line -> line = "")

    if splitIndex.IsNone then
        None
    else
        let (rules, pages) = lines |> Array.splitAt splitIndex.Value
        let computedRules = rules |> Array.map readRule
        let computedPages = pages |> Array.tail |> Array.map readPage
        Some(computedRules, computedPages)

let isRuleApplicable update (ruleFirst, ruleSecond) =
    let exists rulePart =
        update |> Array.exists (fun page -> page = rulePart)

    exists ruleFirst && exists ruleSecond

let validateUpdate (rules: (int * int) array) (update: int array) =
    let rulesToApply = rules |> Array.filter (isRuleApplicable update)

    if Array.length rulesToApply = 0 then
        true
    else
        let pagesPosition = update |> Array.mapi (fun i x -> (x, i)) |> Map.ofArray

        rulesToApply
        |> Array.forall (fun (a, b) ->
            let aIndex = pagesPosition.[a]
            let bIndex = pagesPosition.[b]
            aIndex < bIndex)

let compute (rules: (int * int) array) (pages: int array array) =
    pages
    |> Array.filter (fun update -> validateUpdate rules update)
    |> Array.sumBy (fun update -> update.[(Array.length update) / 2])

let sample1Data =
    sample1.ByNewLine() |> readData |> Option.get |> (fun (a, b) -> compute a b)

let result1 = Files.[5] |> readData |> Option.get |> (fun (a, b) -> compute a b)

// part two

let orderUpdate (rules: (int * int) array) update =
    let rulesToApply = rules |> Array.filter (isRuleApplicable update)
    let rulesSet = rulesToApply |> Array.map (fun (a, b) -> (a, b)) |> Set.ofArray

    let comparer a b =
        if Set.contains (a, b) rulesSet then -1
        elif Set.contains (b, a) rulesSet then 1
        else 0

    update |> Array.sortWith comparer

let compute2 (rules: (int * int) array) (pages: int array array) =
    pages
    |> Array.filter (not << (fun update -> validateUpdate rules update))
    |> Array.sumBy ((orderUpdate rules) >> (fun update -> update.[(Array.length update) / 2]))

let sampleRules = sample1.ByNewLine() |> readData |> Option.get |> (fun (a, _) -> a)
let samplePages = sample1.ByNewLine() |> readData |> Option.get |> (fun (_, b) -> b)

let sample2Data =
    sample1.ByNewLine() |> readData |> Option.get |> (fun (a, b) -> compute2 a b)

let result2 = Files.[5] |> readData |> Option.get |> (fun (a, b) -> compute2 a b)
