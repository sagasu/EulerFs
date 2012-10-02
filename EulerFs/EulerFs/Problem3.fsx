#light
open System 

// Not fast enought solution
let largestPrimeFactor (number:decimal) =
    let rec getLargestFactor (number:decimal) (startNumber:decimal) = 
        match number with
        | a when (a % startNumber = 0M) -> startNumber
        | _ -> getLargestFactor number (startNumber - 1M)
    let isPrime (n:decimal) = 
        match n with
        | a when a = 1M || a = 2M -> true
        | _ -> match getLargestFactor n (n / 2M) with
                | 1M -> true
                | _ -> false
    let rec getLargestPrimeFactor startNumber =
        let factor = getLargestFactor number startNumber 
        match factor with
        | a when isPrime factor -> factor
        | _ -> getLargestPrimeFactor (factor - 1M)
    getLargestPrimeFactor( number / 2M)

// Good solution
let rec maxPrimeFactor (number:decimal) (divider:decimal) = 
    if divider * divider > number then
        number
    elif number % divider = 0M then
        maxPrimeFactor (number / divider) divider
    else
        maxPrimeFactor number (divider + 1M)
        
maxPrimeFactor 600851475143M 2M