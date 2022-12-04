let testLines = [ "A Y"; "B X"; "C Z" ]
let lines = System.IO.File.ReadLines("input.txt")

let movescore move =
    match move with
    | "A"
    | "X" -> 1
    | "B"
    | "Y" -> 2
    | "C"
    | "Z" -> 3
    | _ -> failwith "bad input"

let getmoves (line: string) =
    match line.Split(' ') with
    | [| oppMove; myMove |] -> movescore oppMove, movescore myMove
    | _ -> failwith "bad input"


let roundscore (oppMove, myMove) =
    let score =
        match myMove - oppMove with
        // draw
        | 0 -> 3
        // win
        | 1
        | -2 -> 6
        // loss
        | -1
        | 2 -> 0
        | _ -> failwith "bad input"

    score + myMove

let createwinningmove (oppMove, outcome) =
    match outcome with
    | 1 -> (oppMove - 1 + 2) % 3 + 1
    | 2 -> oppMove
    | 3 -> oppMove % 3 + 1
    | _ -> failwith "badinput"

let gamescore1 lines =
    lines
    |> Seq.map (getmoves >> roundscore)
    |> Seq.sum

let gamescore2 lines =
    lines
    |> Seq.map (
        getmoves
        >> (fun (oppMove, outcome) -> oppMove, createwinningmove (oppMove, outcome))
        >> roundscore
    )
    |> Seq.sum

let part1TestResult = testLines |> gamescore1
let part1TestAnswer = 15
printfn "part1test: %d" part1TestResult
assert (part1TestResult = part1TestAnswer)

let part1Result = lines |> gamescore1
let part1Answer = 13682
printfn "part1: %d" part1Result
assert (part1Answer = part1Result)

let part2TestResult = testLines |> gamescore2
let part2TestAnswer = 12
printfn "part2test: %d" part2TestResult
assert (part2TestResult = part2TestAnswer)

let part2Result = lines |> gamescore2
let part2Answer = 12881
printfn "part2: %d" part2Result
assert (part2Result = part2Answer)

let fast1 line =
    match line with
    | "B X" -> 1
    | "C Y" -> 2
    | "A Z" -> 3
    | "A X" -> 4
    | "B Y" -> 5
    | "C Z" -> 6
    | "C X" -> 7
    | "A Y" -> 8
    | "B Z" -> 9
    | _ -> failwith "bad input"

let fast2 line =
    match line with
    | "B X" -> 1
    | "C X" -> 2
    | "A X" -> 3
    | "A Y" -> 4
    | "B Y" -> 5
    | "C Y" -> 6
    | "C Z" -> 7
    | "A Z" -> 8
    | "B Z" -> 9
    | _ -> failwith "bad input"

let fastpart1 = lines |> Seq.map fast1 |> Seq.sum
printfn "fast part1: %d" fastpart1

let fastpart2 = lines |> Seq.map fast2 |> Seq.sum
printfn "fast part2: %d" fastpart2
