open System
open System.IO

let testArr = [| 0 ; 1 ; 2 ; 3 |]

let safeIndexIf (arr : 'a []) i =
  if i < 0 || i > Array.length arr - 1 then
    Unchecked.defaultof<'a>
  else
    arr.[i]

printfn "Index on edge test:      %i" <| safeIndexIf testArr 0
printfn "Index on edge test:      %i" <| safeIndexIf testArr 3

let safeIndexTry (arr : 'a []) (i : int) =
  try
    arr.[i]
  with
  | _ -> failwith "Index out of bound"

try safeIndexTry testArr -1
with | _ -> try safeIndexTry testArr 4 with | _ -> printfn "Index out of bound: true"; 0

printfn "Index on edge test:      %i" <| safeIndexTry testArr 0
printfn "Index on edge test:      %i" <| safeIndexTry testArr 3

let safeIndexOption (arr : 'a []) (i : int) =
    if not (i < 0 || i > Array.length arr - 1)
    then Some(arr.[i])
    else None

printfn "Index on edge test:      %i" <| match safeIndexOption testArr 0 with
                                         | Some (i) -> i
                                         | None     -> -1
printfn "Index on edge test:      %i" <| match safeIndexOption testArr 3 with
                                         | Some (i) -> i
                                         | None     -> -1
printfn "Index out of bound:      %b" <| match safeIndexOption testArr 30 with
                                         | Some (i) -> false
                                         | None     -> true


let fileReplace filename needle replace =
  let path = @"./" + filename
  let str = System.IO.File.ReadAllText path

  //Finds first occurance of needle at idx or after
  let rec needleFinder idx =
    if idx < String.length str - String.length needle + 1 then
         if str.[idx .. idx + String.length needle - 1] = needle
         then Some (idx)
         else needleFinder <| idx + 1
    else None

  //Replaces first occurance of needle at or after idx with replace in str
  let rec Replacer idx =
    match needleFinder idx with
    | Some (i) -> str.[0 .. i - 1] + replace + (Replacer <| i + String.length needle)
    | None     -> str.[idx .. ]

  System.IO.File.WriteAllText (path, Replacer 0)

// Jeg går ud fra at du kører programmet fra mappen hvor filen ligger
fileReplace "test.txt" "ikke" "godt"
printfn "test.txt test: %s" (File.ReadAllText @"./test.txt")

let url2Stream url =
  let uri = System.Uri url
  let request = System.Net.WebRequest.Create uri
  let response = request.GetResponse ()
  response.GetResponseStream ()

let countLinks url =
  let str = (new System.IO.StreamReader (url2Stream url)).ReadToEnd()
  let a = "<a"
  let rec finder idx =
    if idx < String.length str - String.length a + 1 then
      if str.[idx .. idx + String.length a - 1] = a
      then finder (idx + String.length a) + 1
      else finder (idx + 1)
    else 0
  finder 0

printfn "%i" <|countLinks "http://dr.dk"
