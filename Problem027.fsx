#load "primality.fs"

let score(a,b) =
    let seq = Seq.initInfinite (fun n -> n*n + a*n + b)
    seq
        |> Seq.takeWhile (fun n -> Primality.is_prime(int64 n))
        |> Seq.length

score(1,41)
 
let solutions = seq {
    for a in -1000..1000 do
        for b in -1000..1000 do
            yield (a, b, score(a,b))
}

let (a,b,maxScore) = solutions |> Seq.maxBy (fun (a,b,score) -> score)
let solution = a*b