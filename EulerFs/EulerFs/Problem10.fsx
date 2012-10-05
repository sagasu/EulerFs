#light

let isOddPrime n =
    let rec isOddPrime divider =
        match divider with
        | 1 -> true
        | a when n % divider = 0 -> false
        | _ -> isOddPrime (divider - 1)
    isOddPrime (int (System.Math.Sqrt(float n)))

let sumPrimes =
    let rec sumPrimes (n:decimal) (sum:decimal) =
        match n with
        | a when a >= 2000000M -> sum //2000000
        | _ -> 
            if isOddPrime (int n) then
                sumPrimes (n + 2M) (sum + n)
            else
                sumPrimes (n + 2M) sum
    sumPrimes 3M 2M