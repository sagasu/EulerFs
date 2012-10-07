let (cash:int []) = Array.zeroCreate 1000001
let rec getGollatzNumber (origNumber:decimal) stepNumber (n:decimal) =
    //System.Console.WriteLine("{0} {1} {2}", origNumber, stepNumber, n)
    match n  with
    | 1M -> 
        cash.[int origNumber] <- stepNumber
        (stepNumber, origNumber)
    | a when a < origNumber -> 
        cash.[int origNumber] <- stepNumber + cash.[int n] - 1
        (cash.[int n] + stepNumber, origNumber)
    | _ ->
        match (n % 2M = 0M) with
        | true -> getGollatzNumber origNumber (stepNumber + 1) (n/2M)
        | false -> getGollatzNumber origNumber (stepNumber + 1) (3M * n + 1M)

seq {2M .. 1000000M}
    |> Seq.fold (fun (steps, number) x -> 
        let nextNumberSteps, nextNumber = (getGollatzNumber x 1 x)
        if nextNumberSteps > steps then
            (nextNumberSteps, nextNumber)
        else
            (steps, number)
        ) (1, 1M)