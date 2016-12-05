open System
open System.IO


let rec fac = function
  | 0            -> 1
  | n when n < 0 -> invalidArg "n" "fac is not defined for negative numbers"
  | n            -> fac (n - 1) * n

let facN = function
  | n when n < 0 -> Some (fac n)
  | _ -> None

let printFile ()  =
  printf "Skriv navnet på din fil: "
  let fileName = System.Console.ReadLine ()
  let path = @"C:\Users\Frederik Mastratisi\Dropbox\@DATALOGI\Opgaver\PoP\Ovelser\"
  try
    printf "%s" (String.concat "\n" (File.ReadAllLines (path + fileName)))
  with
  | _ -> invalidArg "fileName" "Not a valid filename"

let url2Stream url =
  let uri = System.Uri url
  let request = System.Net.WebRequest.Create uri
  let response = request.GetResponse ()
  response.GetResponseStream ()

let printWebPage URL =
  let stream = System.IO.StreamReader (url2Stream URL)
  let page = stream.ReadToEnd ()
  stream.Close()
  page

let operator = function
| n when n = '+' -> (+)
| n when n = '-' -> (-)
| n when n = '*' -> (*)
| n when n = '/' -> (/)
| n when n = '%' -> (%)
| _ -> invalidArg "operator function" "Not a valid operator"

let isOperator = function
| n when n = '+' -> true
| n when n = '-' -> true
| n when n = '*' -> true
| n when n = '/' -> true
| n when n = '%' -> true
| _ -> false

let isSpace = function
| n when n = " " -> true
| _ -> false

let isNumber n = List.contains n (List.map (string >> char) [0 .. 9])

let rec spaceFinder str i =  // Finds fist space at or after i
  if String.length str > i + 1 then
    match str.[i] with
    | ' ' -> Some (i)
    | _   -> spaceFinder str (i + 1)
  else None

let rec operatorFinder (str : string) i =
  try match isOperator str.[i] with
      | true  -> i
      | false -> operatorFinder str (i + 1)
  with
  | _ -> 0


let calculator () =
  System.Console.WriteLine "Kill process to quit..."
  let rec calc () =
      System.Console.Write "Enter calculations: "
      let input = String.filter (fun c -> (isOperator c || isNumber c)) (System.Console.ReadLine ())
      let indxPosOp = operatorFinder input 0
      let lhs = float (input.[0 .. indxPosOp - 1])
      let rhs = float (input.[indxPosOp + 1 .. ])
      System.Console.WriteLine ("\n" + string (operator input.[indxPosOp] lhs rhs))
      calc ()
      ()
  calc ()
calculator ()
