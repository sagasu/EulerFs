open System.Linq

// 670442572800M
let smallestProduct = 20M * 19M * 18M * 17M * 16M * 15M * 14M * 13M * 12M * 11M

// Brute Force 232792560M - correct
let bruteForceSolver = 
    let check n = 
        let rec check checkAgainst = 
            match checkAgainst with
            | 21M -> true
            | a when (n % checkAgainst = 0M) -> check (checkAgainst + 1M)
            | _ -> false
        check 11M
    let rec bruteForceSolver n =
        match n with
        | a when check n -> n
        | _ -> bruteForceSolver (n + 2520M)
    bruteForceSolver 2520M

let rec gcd x y = 
    match y with
    | 0 -> x
    | _ -> gcd y (x % y)
        
let lcm x y =
    (x * y) / gcd x y

// 18044195  [1 .. 20]
let solveByFold =
    let collection = [11 .. 20]
    Seq.fold (fun x y -> lcm x y) 1 collection