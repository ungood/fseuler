// Calculates the "optimal" order of completing Project Euler problems
// to complete achievements the fastest.

#load "primality.fs"
#load "sequences.fs"

let N = 429

let solved = set [ 1;  2;  3;  4;  5;  6;  7;  8;  9; 10;
                  11; 12; 13; 14; 15; 16;     18;     20;
                  21;                     27;
                                          67;
                                                  79;
                  81]

let pi = [3; 14; 15; 92; 65; 35; 89; 79; 32; 38; 46]
let trinary = [1; 3; 9; 27; 81; 243]

let fib =
    Seq.unfold (fun (x,y) -> Some(x, (y, x+y))) (1,2)
    |> Seq.take 12
    |> Seq.toList

let squared =
    [1..int(sqrt(float N))]
    |> List.map (fun x -> x*x)

let prime =
    Sequences.Primes
    |> Seq.take 50
    |> Seq.toList

let lucky =
    Sequences.Lucky
    |> Seq.take 25
    |> Seq.toList

let triangle =
    Sequences.TriangleNumbers
    |> Seq.take 25
    |> Seq.toList

let daring = [100..N]

let recent x = [N-(x-1)..N]

// manually input the problems solved by less than 100 people.
let gold = [427; 428; 415; 426; 422]

let all =
    [pi; trinary; fib; squared; prime; lucky; triangle; daring; recent(1); recent(5); recent(25); gold]
    |> List.map set
    |> List.map (fun s -> s - solved)

let score x = 
    let s(achieve:Set<int>) =
        if achieve.IsEmpty
            then 0.0
        else if achieve.Contains(x)
            then 1.0 / float(achieve.Count)
            else 0.0
    all |> List.sumBy s
        
[1..N]
|> List.map (fun problem -> (problem, score problem))
|> List.sortBy (fun (problem, score) -> score)
|> List.rev
