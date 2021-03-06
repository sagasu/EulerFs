﻿let sumOfFactors n sqrtOfNumber sum = 
    Seq.fold (fun acc x -> if (n % x) = 0 then acc + x + (n / x) else acc) sum [2 .. sqrtOfNumber]

let sumOfFactors1 (n:int) = 
    let sqrtOfNumber = int (System.Math.Sqrt (float n))
    if n = sqrtOfNumber * sqrtOfNumber then
        sumOfFactors n (sqrtOfNumber - 1) (sqrtOfNumber + 1)
    else
        (sumOfFactors n sqrtOfNumber 1)

let problem21 uppLimit= 
    Seq.fold (fun acc x -> 
        let faci = (sumOfFactors1 x)
        if faci > x && faci <= uppLimit then
            let facj = sumOfFactors1 faci
            if facj = x then 
                acc + x + faci
            else
                acc
        else
            acc)
        0
        [2..uppLimit]