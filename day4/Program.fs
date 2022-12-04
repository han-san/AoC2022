let input = System.IO.File.ReadLines("input.txt")

let testinput =
    [ "2-4,6-8"
      "2-3,4-5"
      "5-7,7-9"
      "2-8,3-7"
      "6-6,4-6"
      "2-6,4-8" ]

let overlaps ((leftStart, leftEnd), (rightStart, rightEnd)) =
    leftStart <= rightEnd && leftEnd >= rightStart

let contains ((leftStart, leftEnd), (rightStart, rightEnd)) =
    leftStart >= rightStart && leftEnd <= rightEnd

let eitherContains (left, right) =
    contains (left, right) || contains (right, left)

let parseLine (line: string) =
    line.Split [| '-'; ',' |]
    |> Array.map int
    |> function
        | [| a; b; c; d |] -> (a, b), (c, d)
        | _ -> failwith "bad input"

let parseInput = Seq.map parseLine

let solveInputWithFilter filter =
    parseInput >> Seq.filter filter >> Seq.length

let part1: seq<string> -> int = solveInputWithFilter eitherContains

let part2: seq<string> -> int = solveInputWithFilter overlaps

let part1TestAnswer = 2
let part1TestResult = part1 testinput
printfn "part1 test: %d" part1TestResult
assert (part1TestAnswer = part1TestResult)

let part1Answer = 450
let part1Result = part1 input
printfn "part1: %d" part1Result
assert (part1Answer = part1Result)

let part2TestAnswer = 4
let part2TestResult = part2 testinput
printfn "part2 test: %d" part2TestResult
assert (part2TestAnswer = part2TestResult)

let part2Answer = 837
let part2Result = part2 input
printfn "part2: %d" part2Result
assert (part2Answer = part2Result)
