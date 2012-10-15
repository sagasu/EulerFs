open System;

let problem19 =
    Seq.fold (fun acc year -> 
                Seq.fold (fun monthAcc month -> if (new DateTime(year, month, 1)).DayOfWeek = DayOfWeek.Sunday then monthAcc + 1 else monthAcc)
                         acc 
                         (seq {1 .. 12}))
            0 (seq {1901 .. 2000})