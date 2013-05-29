module Sequences

open Primality

let Naturals = Seq.initInfinite (fun x -> x+1)
let OddNaturals = Naturals |> Seq.filter (fun x -> x % 2 <> 0)

let _luckySieve(remaining:seq<_>, step) =
    remaining
    |> Seq.mapi (fun i el -> (i+1, el))
    |> Seq.filter (fun (i, el) -> i % step <> 0)
    |> Seq.map (fun (i, el) -> el)

let _luckyGen((remaining:seq<_>, i)) =
    let next = remaining |> Seq.nth i
    let remaining' = _luckySieve(remaining, next)
    Some(next, (remaining', i+1))

let Lucky = seq {
    yield 1
    yield! Seq.unfold (_luckyGen) (OddNaturals, 1)
}

let Primes =
    Naturals
    |> Seq.filter (fun n -> is_prime(int64(n)))

let TriangleNumbers =
    Naturals
    |> Seq.map (fun n -> [1..n] |> List.sum)

let Fibonacci = Seq.unfold (fun (x,y) -> Some(x, (y, x+y))) (1,2)