#light

let sumEvenFib (maxIter:int) =
    let rec sumFib (prev:int) (curr:int) (sum:int) = 
        match curr with
        | a when (a < maxIter) && (a % 2 = 1) -> sumFib curr (prev + curr) (sum + curr)
        | a when (a < maxIter) && (a % 2 = 0) -> sumFib curr (prev + curr) sum
        | _ -> sum
    sumFib 1 2 2 // The 2 in sum is a sum of first two even elements in fibonacci sequence
