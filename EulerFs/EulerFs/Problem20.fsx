let (memoize:bigint []) = Array.zeroCreate 100

let rec factorial  (n:bigint) :bigint= 
    match n with 
    | a when a = (bigint 0) -> (bigint 1)
    | a -> 
        if memoize.[(int n)-1] <> (bigint 0) then
            memoize.[(int n)-1]
        else
            memoize.[(int n)-1] <- factorial (n - (bigint 1))
            n * memoize.[(int n)-1]

let problem20 =
    Seq.sum (Seq.map (fun x -> int (x.ToString())) ((factorial (bigint 100)).ToString().ToCharArray()))