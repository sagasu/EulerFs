#light
// a * a + b * b = (1000 - a - b) * (1000 - a - b)

let findPythagorean =
    let rec findPythagorean2 a b =
        match b with
        | 1000 -> None
        | _ -> 
            if a * a + b * b = (1000 - a - b) * (1000 - a - b) then
                Some (a, b, 1000 - a - b)
            else
                findPythagorean2 a (b + 1)
    let rec findPythagorean a =
        match a with
        | 1000 -> None
        | _ -> 
            let pythagorean = findPythagorean2 a 1
            if pythagorean = None then
                findPythagorean (a + 1)
            else
                pythagorean
    findPythagorean 1