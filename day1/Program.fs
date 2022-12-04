// For more information see https://aka.ms/fsharp-console-apps

let input = System.IO.File.ReadAllLines("input.txt")

let low, mid, high, _ =
    input
    |> Array.fold
        (fun sums calories ->
            let low, mid, high, current = sums

            match calories with
            | "" ->
                match [| max low current; mid; high |] |> Array.sort with
                | [| newLow; newMid; newHigh |] -> newLow, newMid, newHigh, 0
                | _ -> failwith "bad input"
            | _ -> low, mid, high, current + (calories |> int))
        (0, 0, 0, 0)

let part1Answer = 64929
let part1Result = high
printfn "part1: %d" high
assert (part1Answer = part1Result)

let part2Answer = 193697
let part2Result = low + mid + high
printfn "part2: %d" part2Result
assert (part2Answer = part2Result)
