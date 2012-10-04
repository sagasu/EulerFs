#light

// 100 * 100 = 10000      - 5 digits
// 999 * 999 = 998001     - 6 digits
// biggest palidrome 997799

let isPalidrome (first, second, third, fourth, fifth, sixth) =
    if first = sixth && second = fifth && third = fourth then
        true
    else
        false     

let GetMaxProduct (a:int) (b:int) (currentMax:int) :int =
    let product = a * b
    let productAsArray = product.ToString().ToCharArray()
    if product > 100000 &&
       (product % 11 = 0) &&
       (System.Math.Max(product,currentMax) = product) &&
       isPalidrome (productAsArray.[0], productAsArray.[1], productAsArray.[2],productAsArray.[3],productAsArray.[4],productAsArray.[5]) then
        product
    else
        currentMax

let GetPalidrome = 
    let mutable maxPalidromeProduct = 0
    for i = 100 to  999 do
        for j = 100 to 999 do
            maxPalidromeProduct <- GetMaxProduct i j maxPalidromeProduct
    maxPalidromeProduct