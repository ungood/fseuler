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

// limit a sequence to only those in the problem set
let limit sequence = sequence |> Seq.takeWhile (fun x -> x <= N)

// defines an achievement unlocked by completing
// any n problems in a list of them.
let any n problems = 
    let problemSet = set problems
    let solvedProblems = Set.intersect problemSet solved
    let remaining = n - solvedProblems.Count
    let score(problem) =
        if problemSet.Contains(problem) <> true || solvedProblems.Contains(problem) || remaining < 1
            then 0.0
            else 1.0 / (float(remaining))
    score

// unlocked by completed all the problems in the sequnce.
let all problems = problems |> any (Seq.length problems)

// unlocked by completing N problems in an infinite sequence.
let first n problems = problems |> Seq.take n |> all

// Solve the most recent X problems.
let recent x = [N-(x-1)..N] |> all

// for ones I haven't done yet...
let todo = (fun x -> 0.0)

let ``Decathlete`` = todo
let ``Centurion`` = todo

let ``As Easy As Pi`` = [3; 14; 15; 92; 65; 35; 89; 79; 32; 38; 46] |> all

// manually input the problems solved by less than 100 people.
let ``One In A Hundred`` = [427; 428; 415; 426; 422] |> any 1

let ``Unlucky Squares`` = Seq.initInfinite (fun x -> x*x) |> limit |> any 13

let ``Prime Obsession`` = Sequences.Primes |> limit |> any 50

let ``Baby Steps`` = [1..N] |> any 3

let ``The Journey Begins`` = [1..N] |> any 25

let ``Perfection`` = [1..N] |> all

let ``Daring Dozen`` = [100..N] |> any 12

let ``On The Ball`` = recent 1
let ``On The Ball II`` = recent 5
let ``State Of The Art`` = recent 25

let ``Trinary Triumph`` = [1; 3; 9; 27; 81; 243] |> all

let ``Fibonacci Fever`` = Sequences.Fibonacci |> first 12

let ``Triangle Trophy`` = Sequences.TriangleNumbers |> first 25

let ``Lucky Luke`` = Sequences.Lucky |> limit |> any 50

let ``Decimation I`` = todo
let ``Decimation II`` = todo
let ``Ultimate Decimator`` = todo

let achievements = [
    ``Decathlete``;
    ``Centurion``;
    ``As Easy As Pi``;
    ``One In A Hundred``;
    ``Unlucky Squares``;
    ``Prime Obsession``;
    ``Baby Steps``;
    ``The Journey Begins``;
    ``Perfection``;
    ``Daring Dozen``;
    ``On The Ball``;
    ``On The Ball II``;
    ``State Of The Art``;
    ``Trinary Triumph``;
    ``Fibonacci Fever``;
    ``Triangle Trophy``;
    ``Lucky Luke``;
    ``Decimation I``;
    ``Decimation II``;
    ``Ultimate Decimator``
]

let score problem = 
    achievements
    |> Seq.sumBy (fun achievement -> (achievement problem))
        
let results =
    [1..N]
    |> List.map (fun problem -> (problem, score problem))
    |> List.sortBy (fun (problem, score) -> score)
    |> List.rev