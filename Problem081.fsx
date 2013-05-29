let example = array2D [
    [ 131; 673; 234; 103;  18 ];
    [ 201;  96; 342; 965; 150 ];
    [ 630; 803; 746; 422; 111 ];
    [ 537; 699; 497; 121; 956 ];
    [ 805; 732; 524;  37; 331 ];
]

let solve(a:int[,]) =
    let width = a.GetLength(1)
    let height = a.GetLength(0)
    let score(x, y) =
        match (x,y) with
        | (x,y) when x < width-1 && y < height-1 -> a.[y,x] + min a.[y+1,x] a.[y,x+1]
        | (x,y) when x < width-1 -> a.[y,x] + a.[y,x+1]
        | (x,y) when y < height-1 -> a.[y,x] + a.[y+1,x]
        | (x,y) -> a.[y,x]
    for y in (height-1).. -1 ..0 do
        for x in (width-1).. -1 ..0 do
            a.[y,x] <- score(x,y)
    a.[0,0]

let example_solution = solve(example)

#load "Problem081_Data.fs"

let solution = solve(Problem081_Data.problem_data)