module Primality

// TODO: Write my own instead of copying this.
let is_prime n =
    let maxFactor = int64(sqrt(float n))
    let rec loop testPrime tog =
        if testPrime > maxFactor then true
        elif n % testPrime = 0L then false
        else loop (testPrime + tog) (6L - tog)
    if n = 2L || n = 3L || n = 5L then true
    elif n <= 1L || n % 2L = 0L || n % 3L = 0L || n % 5L = 0L then false
    else loop 7L 4L
