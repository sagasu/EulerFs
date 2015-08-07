#light

// brute force really costly, not fast countDivisors alg. 76576500
let getTriangle =  
    let rec countDivisors n numberOfDivisors divisor =
        match divisor with
        | 1 -> (numberOfDivisors + 2) // divided by one and itself
        | _ ->
            match n % divisor = 0 with
            | true -> countDivisors n (numberOfDivisors + 1) (divisor - 1)
            | _ -> countDivisors n numberOfDivisors (divisor - 1)
    let hasMoreThen500Divisors n =
        match countDivisors n 0 (int (n / 2)) with
        | a when a > 500 -> true
        | _ -> false
    let rec getTriangle n triangleNumber = 
        match hasMoreThen500Divisors triangleNumber with
        | true -> triangleNumber
        | _ -> getTriangle (n + 1) (triangleNumber + n + 1)
    getTriangle 3 6

// Much faster brute force
let getTriangle2 =  
    let numberOfDivisors n =
        let sqrt = int (System.Math.Sqrt(float n)) 
        let nod = seq {1 .. sqrt} |> Seq.filter (fun x -> n % x = 0) |> Seq.fold (fun acc x -> acc + 2) 0

        if sqrt * sqrt = n then
            nod - 1
        else
            nod
    let hasMoreThen500Divisors n =
        //System.Console.WriteLine("{0} {1}", n, (numberOfDivisors n))
        match numberOfDivisors n with
        | a when a > 500 -> true
        | _ -> false
    let rec getTriangle n triangle = 
        let hasMore = (hasMoreThen500Divisors triangle)
        match hasMore with
        | true -> triangle
        | _ -> getTriangle (n + 1) (triangle + n)
    getTriangle 1 0

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

