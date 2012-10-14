//137846528820
let (memoize:float []) = Array.zeroCreate 40
let problem15Bis =
    let rec factorial  n :float= 
        match n with 
        | 0 -> 1.
        | a -> 
            if memoize.[n-1] <> 0.0 then
                memoize.[n-1]
            else
                memoize.[n-1] <- factorial (n - 1)
                (float n) * memoize.[n-1]
    let fac40 = factorial 40
    let fac20 = factorial 20
    (fac40, fac20, fac40 / (fac20 * fac20))