let problem22 = 
    using(new System.IO.StreamReader "names.txt") (fun r -> 
        let line = r.ReadToEnd()
        line)