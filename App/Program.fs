module IntOption = 
    let plus a b = 
        match a, b with 
        | Some i, Some j -> Some (i + j)
        | _ -> None

    let (++) a b = plus a b

type Sum = 
    {
        IsComplete: bool
        PartialSum: int
        TotalSum: int option
    }

module Sum = 
    open IntOption

    let calculate (a: int option) (b: int option) (c: int option) = 
        {
            IsComplete = a.IsSome && b.IsSome && c.IsSome
            PartialSum = 
                (a |> Option.defaultValue 0)
                + (b |> Option.defaultValue 0)
                + (c |> Option.defaultValue 0)
            TotalSum = a ++ b ++ c
        }

type Square = 
    {
        a: int option
        b: int option
        c: int option
        d: int option
        e: int option
        f: int option
        g: int option
        h: int option
        i: int option
    }

module Square = 
    open IntOption

    let print s = 
        let printOption = 
            function 
            | Some i -> i.ToString()
            | None -> "_"

        printfn "%s | %s | %s" (printOption(s.a)) (printOption(s.b)) (printOption(s.c))
        printfn "%s | %s | %s" (printOption(s.d)) (printOption(s.e)) (printOption(s.f))
        printfn "%s | %s | %s" (printOption(s.g)) (printOption(s.h)) (printOption(s.i))

    let allPartialSumsAreLessThanConstant sums targetConstant = 
        sums |> List.forall (fun s -> s.PartialSum <= targetConstant)

    let allTotalSumsEqualConstant sums targetConstant = 
        sums |> List.where (fun s -> s.TotalSum.IsSome) |> List.forall (fun s -> s.TotalSum.Value = targetConstant)

    let isValidForConstant s targetConstant = 
        let s1 = Sum.calculate s.a s.b s.c
        let s2 = Sum.calculate s.d s.e s.f
        let s3 = Sum.calculate s.g s.h s.i
        let s4 = Sum.calculate s.a s.d s.g
        let s5 = Sum.calculate s.b s.e s.h
        let s6 = Sum.calculate s.c s.f s.i
        let s7 = Sum.calculate s.a s.e s.i
        let s8 = Sum.calculate s.g s.e s.c

        let sums = [ s1; s2; s3; s4; s5; s6; s7; s8 ]
        
        allPartialSumsAreLessThanConstant sums targetConstant
        && allTotalSumsEqualConstant sums targetConstant

    let isComplete s = 
        s.a.IsSome
        && s.b.IsSome
        && s.c.IsSome
        && s.d.IsSome
        && s.e.IsSome
        && s.f.IsSome
        && s.g.IsSome
        && s.h.IsSome
        && s.i.IsSome

    let create a b c d e f g h i = 
        {
            a = a
            b = b
            c = c
            d = d
            e = e
            f = f
            g = g
            h = h
            i = i
        }


// let generateMagicSquares magicConstant = 
//    addNextIntToNextSquare()
//    CheckPossible()
//    if not possible then backtrack
//    else 

type BacktrackingState = 
    {
        mutable LastIncremented: int
        Guesses: Map<int, int>
    }

let initialiseBacktrackingState() = 
    {
        LastIncremented = 0
        Guesses = 
            [
                (0, 0)
                (1, 0)
                (2, 0)
                (3, 0)
                (4, 0)
                (5, 0)
                (6, 0)
                (7, 0)
                (8, 0)
            ] |> Map
    }

let buildSquare state = 
    let a = if state.Guesses[0] = 0 then None else Some state.Guesses[0]
    let b = if state.Guesses[1] = 0 then None else Some state.Guesses[1]
    let c = if state.Guesses[2] = 0 then None else Some state.Guesses[2]
    let d = if state.Guesses[3] = 0 then None else Some state.Guesses[3]
    let e = if state.Guesses[4] = 0 then None else Some state.Guesses[4]
    let f = if state.Guesses[5] = 0 then None else Some state.Guesses[5]
    let g = if state.Guesses[6] = 0 then None else Some state.Guesses[6]
    let h = if state.Guesses[7] = 0 then None else Some state.Guesses[7]
    let i = if state.Guesses[8] = 0 then None else Some state.Guesses[8]
    Square.create a b c d e f g h i        


let tryNext state targetConstant = 
    let maxGuess = targetConstant - 2

    let previousGuess = state.Guesses[state.LastIncremented]

    if previousGuess = maxGuess 
        then 
            let lastIncremented = state.LastIncremented
            { state with 
                LastIncremented = state.LastIncremented - 1
                Guesses = Map.change lastIncremented (fun _ -> Some 0) state.Guesses
            }
        else 
            { state with 
                Guesses = Map.change state.LastIncremented (fun i -> IntOption.plus i (Some 1)) state.Guesses
            }

let backTrack targetConstant = 
    let mutable state = initialiseBacktrackingState()
    let mutable escape = false
    let mutable finalAnswer : Square option = None

    while not escape do
        let nextGuess = tryNext state targetConstant
        let nextSquare = buildSquare nextGuess

        printfn "trying out this square:"
        Square.print nextSquare

        if Square.isComplete nextSquare && Square.isValidForConstant nextSquare targetConstant then 
            finalAnswer <- Some nextSquare
            escape <- true
        else 
            state <- nextGuess

    finalAnswer

let ret = backTrack 15

printfn "Found something"
Square.print ret.Value


// let s =
//     {
//         a = Some 1
//         b = Some 2
//         c = Some 2
//         d = Some 2
//         e = Some 2
//         f = Some 2
//         g = Some 2
//         h = Some 2
//         i = None
//     }

// Square.print s    