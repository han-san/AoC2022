let input = System.IO.File.ReadLines("input.txt")

let testinput =
    [ "vJrwpWtwJgWrhcsFMMfFFhFp"
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
      "PmmdzqPrVvPwwTWBwg"
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
      "ttgJtRGJQctTZtZT"
      "CrZsJsPPZsGzwwsLwLmpwMDw" ]

let priorityTable = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

type item = char
type compartment = seq<item>
type rucksack = seq<compartment>
type rucksacklist = seq<rucksack>
type group = rucksack

let commonItem: rucksack -> item =
    Seq.map set
    >> Seq.reduce Set.intersect
    >> Seq.exactlyOne

let priorityOfItem: item -> int = priorityTable.IndexOf >> (+) 1

let solve: rucksacklist -> int = Seq.map (commonItem >> priorityOfItem) >> Seq.sum

let createrucksack (str: string) : rucksack = str |> Seq.splitInto 2 |> Seq.map seq

let creategroups: seq<string> -> seq<group> =
    Seq.chunkBySize 3 >> Seq.map (seq >> Seq.map seq)

let part1: seq<string> -> int = Seq.map createrucksack >> solve

let part2: seq<string> -> int = creategroups >> solve

let part1TestAnswer = 157
let part1TestResult = part1 testinput
printfn "part1 test: %d" part1TestResult
assert (part1TestAnswer = part1TestResult)

let part1Answer = 8349
let part1Result = part1 input
printfn "part1: %d" part1Result
assert (part1Answer = part1Result)

let part2TestAnswer = 70
let part2TestResult = part2 testinput
printfn "part2 test: %d" part2TestResult
assert (part2TestAnswer = part2TestResult)

let part2Answer = 2681
let part2Result = part2 input
printfn "part2: %d" part2Result
assert (part2Answer = part2Result)
