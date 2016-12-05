let xs = [1;2;7;2;1;8;2]

let rec removeDuplicate = function
    |x::xs -> x::removeDuplicate(List.filter(fun y -> y<>x) xs)
    |_ -> []

printfn "%A" (removeDuplicate xs)
