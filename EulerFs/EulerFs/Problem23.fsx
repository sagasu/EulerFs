//A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
//A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
//As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
//Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

// returns a list of all prime numbers <= n
let sieveOfEratosthenes n = 
    let rec sieveOfEratosthenes n p filteredSeq =
        match p * p > n with
        | true -> filteredSeq
        | false -> 
            let filSeq = filteredSeq |> Seq.filter (fun x -> (x % p <> 0) || (x = p))
            //System.Console.WriteLine("{0} {1}", p, Seq.fold (fun acc x -> acc + " " + x.ToString()) "" filSeq)
            sieveOfEratosthenes n (Seq.nth ((Seq.findIndex (fun x -> x = p) filSeq) + 1) filSeq) filSeq
    sieveOfEratosthenes n 2 (seq {2 .. n})


let sumOfFactors n sqrtOfNumber sum = 
    Seq.fold (fun acc x -> if (n % x) = 0 then acc + x + (n / x) else acc) sum [2 .. sqrtOfNumber]

let sumOfFactors1 (n:int) = 
    let sqrtOfNumber = int (System.Math.Sqrt (float n))
    if n = sqrtOfNumber * sqrtOfNumber then
        sumOfFactors n (sqrtOfNumber - 1) (sqrtOfNumber + 1)
    else
        (sumOfFactors n sqrtOfNumber 1)

let isAbudent n =
    sumOfFactors1 n > n

let allAbudentNumbers = 
    [12 .. 28123] |> Seq.filter(fun x -> isAbudent x) 

let canNotBeWritenAsAumOfAbudentNumbers n =
    allAbudentNumbers |> Seq.filter(fun x -> x < n/2) 
    true

let sumNonAbundantNumbers = 
    [0 .. 28123] |> Seq.filter(fun x -> canNotBeWritenAsAumOfAbudentNumbers x) |> Seq.sum(fun x -> x)