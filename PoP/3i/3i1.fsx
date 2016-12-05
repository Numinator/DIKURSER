/// <summary>
///   Generates a multiplication table that colloum lengt of 10 and row depth of
///   n.
/// </summary>
/// <remarks>
///   Input must be a positive integer
/// </remarks>
/// <param name="n">
///   integer; value of the the row depth.
/// </param name="n">
/// <returns>
///   Return an 10 x n multiplication table
/// </returns>


let loopMulTableHelper i j =
  match (i, j) with
   | (0,_) ->  j
   | (_,0) ->  i
   | (_,_) -> j*i


let loopMulTable n =
  let mutable retString  = ""
  for i = 0 to n do
     for j = 0 to 10 do
       retString  <- retString + (sprintf "%4d" (loopMulTableHelper i j))
     retString <- retString + "\n"
  retString

/// <summary>
///   Generates a multiplication table that collum lengt of 10 and row depth of
///   n.
/// </summary>
/// <remarks>
///   Input must be a positive integer and not over the value 10.
/// </remarks>
/// <param name="n">
///   integer; value of the the row depth.
/// </param name="n">
/// <returns>
///   Return an 10 x n multiplication table
/// </returns>

let mulTable n =
 if n <= 10 then
  let mutable table = loopMulTable 10
  table.[0..4*11*(n+1)+n]
 else
  "Du kan ikke kalde mulTable med tal over 10"

/// <summary>
///   Generates a multiplication table that collum lengt of 10 and row depth of
///   n.
/// </summary>
/// <remarks>
///   Input must be a positive integer.
/// </remarks>
/// <param name="n">
///   integer; value of the the row depth.
/// </param name="n">
/// <returns>
///   Return an 10 x n multiplication table
/// </returns>

let recMulTable n =

 let rec rowMaker tal antal =
  if antal < 0 then "" else rowMaker tal (antal - 1) + sprintf "%4i" (tal * antal)

 let rec builder = function
 | 0 -> rowMaker 1 10 + "\n"
 | n -> builder (n-1) + rowMaker n 10  + "\n"

 builder n
/// <summary>
///   compares the result of mulTable, loopMulTable and recMulTable.
/// </summary>
/// <remarks>
///   Input must be a positive integer. The test of recMulTable fails as
///   the left-most collum is populated by zeores.
/// </remarks>
/// <param name="n">
///   integer; compares the functions with all posetive integers up to n.
/// </param name="n">
/// <returns>
///   Side-effect: Sends formated rapport of the comparison the the standard
///   output.
///   Returns Value
/// </returns>

let compare n =
 printfn "%2s | %5s | %5s" "n" "loop" "rec"
 for i = 1 to n do
  printfn "%2i | %5b | %5b" i (mulTable i = loopMulTable i) (mulTable i = recMulTable i)

compare 5

// "%A" er ligeglad med typen og printer det hele, mens "%s" tager string
// og printer kun indeholdet af stringen, altsaa uden ""
