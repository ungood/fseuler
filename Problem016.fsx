// 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
// What is the sum of the digits of the number 2^1000?

let Zero = bigint.Zero

// Finds the digits of n, in base b
let rec digits(n:bigint, b:bigint) =
    if n.IsZero
    then []
    else
        let (q,r) = bigint.DivRem(n,b)
        r :: digits(q,b)
        
let x = bigint.Pow(bigint 2, 1000)

let answer =
    digits(x, bigint 10)
    |> List.sum

// golfed
(bigint 2 ** 1000).ToString() |> Seq.sumBy (fun c -> int(c))