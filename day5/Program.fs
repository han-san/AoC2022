let input =
    System.IO.File.ReadLines("input.txt")
    |> Seq.toList

let testinput =
    [ "    [D]    "
      "[N] [C]    "
      "[Z] [M] [P]"
      " 1   2   3 "
      ""
      "move 1 from 2 to 1"
      "move 3 from 1 to 3"
      "move 2 from 2 to 1"
      "move 1 from 1 to 2" ]

let rec splitOnEmptyStringHelper part1 part2 =
    match part2 with
    | "" :: xs -> List.rev part1, xs
    | x :: xs -> splitOnEmptyStringHelper (x :: part1) xs
    | _ -> failwith "bad input"

let splitOnEmptyString = splitOnEmptyStringHelper []

let initstate (strings: list<string>) =
    strings
    |> Seq.map (fun str -> str.ToCharArray() |> Array.chunkBySize 4)

let createStacks (rows: char [] [] list) : char list [] =
    rows
    |> List.map (Seq.map (Array.item 1))
    |> function
        | x :: xs ->
            xs
            |> List.fold
                (fun columns crates ->
                    columns
                    |> Seq.zip crates
                    |> Seq.map (fun (crate, column) ->
                        if crate <> ' ' then
                            crate :: column
                        else
                            column))
                (x
                 |> Seq.map (fun elem -> [ if elem <> ' ' then elem ]))
            |> Seq.map List.tail
            |> Seq.map List.rev
            |> Seq.toArray
        | _ -> failwith "bad input"

let toString: char list -> string = List.map string >> String.concat ""

let createMoves (moveStrings: list<string>) : seq<int * int * int> =
    moveStrings
    |> Seq.map (fun line ->
        line
        // FIXME: numbers can have multiple digits
        |> Seq.fold
            (fun state c ->
                let strings, currentNumberString = state

                if c >= '0' && c <= '9' then
                    strings, (c :: currentNumberString)
                else if List.isEmpty currentNumberString then
                    strings, currentNumberString
                else
                    ((List.rev currentNumberString) :: strings), [])
            ([], [])
        |> function
            | [ b; a ], c -> a |> toString |> int, b |> toString |> int, c |> toString |> int
            | _ -> failwith "badinput")

let part1 stateInput movesInput =

    let rows = initstate stateInput |> Seq.toList
    let moves = createMoves movesInput
    let stacks = createStacks rows

    moves
    |> Seq.fold
        (fun (currentStacks: list<char> []) (crateCount, fromColumnIndex, toColumnIndex) ->
            let fromColumn = currentStacks[fromColumnIndex - 1]
            let toColumn = currentStacks[toColumnIndex - 1]
            let cratesToMove, newFromColumn = fromColumn |> List.splitAt crateCount
            currentStacks[fromColumnIndex - 1] <- newFromColumn

            currentStacks[toColumnIndex - 1] <- List.concat [ cratesToMove |> List.rev
                                                              toColumn ]

            currentStacks)
        stacks
    |> Seq.map Seq.head
    |> Seq.map string
    |> String.concat ""

let part2 stateInput movesInput =
    let rows = initstate stateInput |> Seq.toList
    let moves = createMoves movesInput
    let stacks = createStacks rows

    moves
    |> Seq.fold
        (fun (currentStacks: list<char> []) (crateCount, fromColumnIndex, toColumnIndex) ->
            let fromColumn = currentStacks[fromColumnIndex - 1]
            let toColumn = currentStacks[toColumnIndex - 1]
            let cratesToMove, newFromColumn = fromColumn |> List.splitAt crateCount
            currentStacks[fromColumnIndex - 1] <- newFromColumn

            currentStacks[toColumnIndex - 1] <- List.concat [ cratesToMove; toColumn ]

            currentStacks)
        stacks
    |> Seq.map Seq.head
    |> Seq.map string
    |> String.concat ""




let teststateInput, testmovesInput = splitOnEmptyString testinput
let stateInput, movesInput = splitOnEmptyString input

let testpart1Answer = "CMZ"
let testpart1Result = part1 teststateInput testmovesInput
assert (testpart1Result = testpart1Answer)

let part1Result = part1 stateInput movesInput
let part1Answer = "VRWBSFZWM"
printfn "%s" part1Result
assert (part1Result = part1Answer)

let testpart2Answer = "MCD"
let testpart2Result = part2 teststateInput testmovesInput
assert (testpart2Result = testpart2Answer)

let part2Result = part2 stateInput movesInput
let part2Answer = "RBTWJWMCF"
printfn "%s" part2Result
assert (part2Result = part2Answer)
