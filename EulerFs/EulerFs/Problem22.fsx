let calculateValue (n:string) =
    Seq.fold (fun (acc:bigint) (x:char) -> (acc+ (bigint(((int)x)-64)))) (bigint 0) n



let problem22 = 
    using(new System.IO.StreamReader @"C:\Users\Mateusz.Kopij\Documents\GitHub\EulerFs\EulerFs\EulerFs\names.txt") (fun r -> 
        let line = r.ReadToEnd()
        let names = line.Split(',')
        let trimmedNames = Seq.map (fun (x:string) -> x.Trim([|'"'|])) names
        let sortedNames = Seq.sort trimmedNames
        Seq.fold (fun (sum:bigint, pos:bigint) x -> ((calculateValue x)*pos, (pos + (bigint 1)))) ((bigint 0), (bigint 1)) sortedNames
        )
