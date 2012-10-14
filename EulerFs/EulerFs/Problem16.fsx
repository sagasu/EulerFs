#light

let problem16 = 
    let result = seq {(bigint 1) .. (bigint 1000)} |> Seq.fold (fun acc x -> acc + acc) (bigint 1)     
    Seq.fold (fun acc x -> 
        acc + (int (x.ToString())))
        0
        (result.ToString().ToCharArray())