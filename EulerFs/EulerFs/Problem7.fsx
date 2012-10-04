let isOddPrime n =
    let rec isOddPrime divider =
        match divider with
        | 1 -> true
        | a when n % divider = 0 -> false
        | _ -> isOddPrime (divider - 1)
    isOddPrime (int (System.Math.Sqrt(float n)))

let findPrimeNth n =
    // start with 3, always odd
    let rec findPrimeNth n start = 
        match n with
        | 1 -> start - 2
        | a ->
            if isOddPrime start then
                findPrimeNth (n - 1) (start + 2)
            else
                findPrimeNth n (start + 2)
    findPrimeNth n 3
