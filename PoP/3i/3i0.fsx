/// <summary>
///   Finds the sum of 1 + 2 ... n - 1 + n with different methods
///   and compare their results with each other in a formated output.
///   The methods used are imperativ, recursive and Gauss's summation methods.
/// </summary>
/// <remarks>
///   Input must be a positive integer
/// </remarks>
/// <param name="n">
///   Integer input taken by all the functions;
///   the sum is taken or compared up to n
/// </param name="n">
/// <returns>
///   Returns either the sum of all natural numbers up to and incluing n
///   (sum, recSum and simpleSum) or returns a comparison of sum's, recSum's
///   and simpleSum's output from 1 to n (compare)
/// </returns>


let sum n =
  let mutable s = 0
  let mutable i = n
  while i >= 0 do
    s <- s + i
    i <- i - 1
  s

let rec recSum = function
  | 0 -> 0
  | n -> n + recSum (n-1)

let simpleSum n =
   (n * (n + 1)) / 2


let compare n =
 printfn "%3s | %4s | %4s | %4s" "n" "sum" "rSum" "sSum"
 for i = 0 to n do
  printfn "%3i | %4i | %4i | %4i" i (sum i) (recSum i) (simpleSum i)
