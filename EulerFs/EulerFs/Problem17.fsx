//let numberLenghts = [ 3; 3; 4; 4; 5; 3; 5; 5; 4; 3 (*ten*); 6;6;8;8;8;7 ]

let nineBase = dict [
                (1, "one".Length);
                (2, "two".Length);
                (3, "three".Length);
                (4, "four".Length);
                (5, "five".Length);
                (6, "six".Length);
                (7, "seven".Length);
                (8, "eight".Length);
                (9, "nine".Length)
                ]

let teen = dict [
                (10, "ten".Length)
                (11, "eleven".Length);
                (12, "twelve".Length);
                (13, "thirteen".Length);
                (14, "fourteen".Length);
                (15, "fifteen".Length);
                (16, "sixteen".Length);
                (17, "seventeen".Length);
                (18, "eighteen".Length);
                (19, "nineteen".Length)
                ]
let tys = dict [
                (20, "twenty".Length);
                (30, "thirty".Length);
                (40, "forty".Length);
                (50, "fifty".Length);
                (60, "sixty".Length);
                (70, "seventy".Length);
                (80, "eighty".Length);
                (90, "ninety".Length);
                ]
let big = dict[
                (100, "hundred".Length);
                (1000, "thousand".Length)]

let problem17 =
    let first99 = 
         Seq.sum (Seq.append nineBase.Values teen.Values)
         +
         (Seq.fold (fun acc x -> acc + 10*x + (Seq.fold (fun accNin nin -> accNin + nin) 0 nineBase.Values)) 0 tys.Values) 
    let first999 = Seq.fold (fun acc x -> acc + ("hundred".Length + x)*100 + first99 + (99*3)) 0 nineBase.Values
    first999 + "thousand".Length
     