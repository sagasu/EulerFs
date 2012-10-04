// (1 .. 100) ** 2 = 5050 ** 2 = 25502500

// 338350M
let sumOfSquares = 
    Seq.fold (fun x y -> x + (y * y)) 0M [1M .. 100M]

// 25502500M
let squareOfSum = 
    let sum = Seq.fold (fun x y -> x + y) 0M [1M .. 100M]
    sum * sum

let difference = squareOfSum - sumOfSquares

