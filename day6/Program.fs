let input = System.IO.File.ReadAllText("input.txt")

let testinputs =
    [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
      "bvwbjplbgvbhsrlpgdmjqwftvncz"
      "nppdvjthqldpwncqszvftbrmjlhg"
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" ]

let test1Answers = [ 7; 5; 6; 10; 11 ]
let test2Answers = [ 19; 23; 23; 29; 26 ]

let tests1 = Seq.zip testinputs test1Answers
let tests2 = Seq.zip testinputs test2Answers

let solve windowSize (input: string) =
    let toIndex c = (c |> int) - ('a' |> int)
    let alphabetSize = toIndex 'z' - toIndex 'a' + 1
    let lookupTable = Array.replicate alphabetSize 0

    input
    |> Seq.take (windowSize - 1)
    |> Seq.iter (fun c -> lookupTable[toIndex c] <- lookupTable[toIndex c] + 1)

    let foo =
        (input
         |> Seq.windowed windowSize
         |> Seq.map (fun window ->
             let headIndex = toIndex (window[0])
             let tailIndex = toIndex (window[windowSize - 1])

             lookupTable[headIndex] <- lookupTable[headIndex] - 1
             lookupTable[tailIndex] <- lookupTable[tailIndex] + 1
             window)
         |> Seq.findIndex (fun window ->
             lookupTable[toIndex window[0]] = 0
             && window
                |> Seq.tail
                |> Seq.forall (fun c -> lookupTable[toIndex c] = 1)))

    foo + windowSize

let part1 = solve 4

let part2 = solve 14

tests1
|> Seq.iter (fun (input, answer) ->
    let testresult = part1 input
    assert (testresult = answer))

let part1result = part1 input
printfn "%d" part1result
let part1Answer = 1658
assert (part1result = part1Answer)

tests2
|> Seq.iter (fun (input, answer) ->
    let testresult = part2 input
    assert (testresult = answer))

let part2result = part2 input
printfn "%d" part2result
let part2Answer = 2260
assert (part2result = part2Answer)
