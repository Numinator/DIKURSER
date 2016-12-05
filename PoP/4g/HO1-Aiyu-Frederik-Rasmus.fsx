printfn "\n****** 4.11 del 1 ******"
/// <summary>
///   Counts the number of times a certain value is occuring in the input list.
/// </summary>
/// <remarks>
///   Input must be a tuble of firstly the list and secondly the value that is
///   being counted.
/// </remarks>
/// <param name="t">
///   Tuple containing a int list and a int.
/// </param name="t">
/// <returns>
///   Returns an interger.
/// </returns>
let rec count = function
    |(x::xs, x') -> (if x = x' then 1 else 0) + count(xs, x')
    |_          -> 0

printfn "%i" (count([1;2;2;3;4;4;5],4))

//Counting an empty list
printfn "Count test1: %b" (count([], 4) = 0)

//Counting a list with one of the same elements
printfn "Count test2: %b" (count([1;2;3;4;5;6], 4) = 1)

//Counting a list with more of the same elements
printfn "Count test3: %b" (count([4;4;5;4;4;4;10], 4) = 5)

//Counting a list with no matching element
printfn "Count test4: %b" (count([1;2;3;5], 4) = 0)


printfn "\n****** 4.11 del 2 ******"
/// <summary>
///   Inserts an element into a sorted list, while keeping the list sorted
/// </summary>
/// <remarks>
///    The input list must be sorted in a non-decending order
/// </remarks>
/// <param name="t">
///   Tuple containing firstly a list and secondly an element.
/// </param name="t">
/// <returns>
///   Returns the input list with the input element inserted in a sorted order.
/// </returns>

let rec insert = function
    |(x::xs, x') when x' > x -> x::insert(xs,x')
    |(xs, x') -> x'::xs
    |(_,x') -> [x']

printfn "%A" (insert ([1;2;3],4))

printfn "%A" (insert ([1;1;4;6;7],5))

//Inserting an element to an empty list
printfn "Insert test1: %b" (insert([], 1) = [1])

//Inserting an element to the end of the list
printfn "Insert test2: %b" (insert([1;2;3;4;5;], 6) = [1;2;3;4;5;6])

//Inserting an element at the the beginning of the list
printfn "Insert test3: %b" (insert([1;2;3;4;5;], 0) = [0;1;2;3;4;5])

//Inserting an element which matches an element in the list
printfn "Insert test4: %b" (insert([1;2;3;4;5;], 3) = [1;2;3;3;4;5])


printfn "\n****** 4.11 del 3 ******"

/// <summary>
///   Makes a new list out of 2 lists, whoose members are the members the input
///   lists have in common
/// </summary>
/// <remarks>
///    The input lists must be sorted in a non-decending order
/// </remarks>
/// <param name="t">
///   Tuple containing 2 lists.
/// </param name="t">
/// <returns>
///   Returns a new list whoose elements are those that the inputs lists have in
///   common
/// </returns>

let rec intersect = function
    | (x::xs, ys) when x = y -> x::intersect(xs,ys)
    | (xs, y::ys) when x < y -> intersect(xs,y::ys)
    | (x::xs, ys)            -> intersect(x::xs,ys)
    |_ -> []

printfn "%A" (intersect ([1;1;1;2;2],[1;1;2;4]))

//Finding the intersection elements for which both of the lists are empty
printfn "Intersect test1: %b" (intersect([],[]) = [])

//Finding the intersection elements for which one of the list is empty
printfn "Intersect test2: %b" (intersect([1;2;3;4], []) = [])

//Finding the intersection elements for a short and long list
printfn "Intersect test3: %b" (intersect([1;1;1;1;4;], [1]) = [1])

//Finding the intersection elements for some sorted random lists
printfn "Intersect test4: %b" (intersect([1;2;3;7;10;33;45], [1;2;5;7;33;45]) = [1;2;7;33;45])

printfn "\n****** 4.11 del 4 ******"

/// <summary>
///   Concatenates 2 sorted lists while keeping the new list sorted.
/// </summary>
/// <remarks>
///    The input lists must be sorted in a non-decending order
/// </remarks>
/// <param name="t">
///   Tuple containing 2 lists.
/// </param name="t">
/// <returns>
///   Returns a new list whoose elements are the same as the input lists in
///   sorted order
/// </returns>

let rec plus = function
    |(xs, [])                      -> xs
    |([], xs')                     -> xs'
    |(x::xs, xs') when x <= x'     -> x::plus(xs, xs')
    |(xs, x'::xs')                 -> x'::plus(xs, xs')
    |_                             -> []

printfn "%A" (plus([1;1;2],[1;2;4]))

//Adding empty lists together
printfn "plus test1: %b" (plus([],[]) = [])

//Adding an empty list to a list with elements
printfn "plus test2: %b" (plus ([1;2;3;4], []) = [1;2;3;4])

//Adding random lists together
printfn "plus test3: %b" (plus([1;2;4;5;6;6;6;99], [1;3;4;7;9;100]) = [1;1;2;3;4;4;5;6;6;6;7;9;99;100])

//Adding only one elements together
printfn "plus test4: %b" (plus([1], [5]) = [1;5])



printfn "\n****** 4.11 del 5 ******"

/// <summary>
///   Makes a new list whoose members are those element who are in the left
///   input list and not in the right input list.
/// </summary>
/// <param name="t">
///   Tuple containing 2 lists.
/// </param name="t">
/// <returns>
///   Returns a new list whose members are those who are in the left list and
///   not in the rigth list.
/// </returns>

let rec minus = function
    |(x::xs, x'::xs') when x = x' -> minus(xs, xs')
    |(x::xs, x'::xs') when x < x' -> x::minus(xs,xs')
    |(x::xs, x'::xs') when x > x' -> minus(x::xs,xs')
    |(xs, [])                     -> xs
    |([], xs')                -> xs'
    |_                            -> []

printfn "%A" (minus([1;1;1;2;2],[1;1;2;3]))

printfn "%A" (minus([1;1;2;3],[1;1;1;2;2]))

//Subtracting lists with no elements
printfn "minus test1: %b" (minus([],[]) = [])

//Subtracting matching lists
printfn "minus test2: %b" (minus ([1;2], [1;2]) = [])

//Subtracting short lists
printfn "minus test3: %b" (minus ([1;1;2], [1;1]) = [2])

//Subtracting long lists
printfn "minus test4: %b" (minus ([1..100], [1..99]) = [100])



printfn "\n****** 4.15 ******"

/// <summary>
///   Reverses the the input list of lists, so that the internal list are
///   reversed and the external one as well.
/// </summary>
/// <param name="xss">
///   'a list list; a list of lists
/// </param name="xss">
/// <returns>
///   Returns the reveserd list of lists
/// </returns>

let rec revrev (xss:'a list list) =
    let rec rev (xs:'a list) =
        match xs with
        |[] -> []
        |y::ys -> (rev ys) @ [y]
    match xss with
    |[] -> []
    |ys::yss -> revrev yss @ [rev ys]

printfn "%A" (revrev [[1;2];[3;4;5]])

//Reversing empty lists
printfn "revrev test1: %b" (revrev [[];[]] = [[];[]])

//Reversing lists with one element
printfn "revrev test2: %b" (revrev [[1];[1]] = [[1];[1]])

//Reversing lists with multiple matching elements
printfn "revrev test3: %b" (revrev [[1;2];[1;2;3]] = [[3;2;1];[2;1]])

//Reversing long lists
printfn "revrev test4: %b" (revrev [[1..19];[20..30]] = [[30..-1..20];[19..-1..1]])

//Reversing lists in the other order
printfn "revrev test5: %b" (revrev [[5;4;3];[2;1]] = [[1;2];[3;4;5]])

/// <summary>
///   Removes duplicates of elements in a list, and keeps the first occurance.
/// </summary>
/// <param name="l">
///   a list.
/// </param name="l">
/// <returns>
///   Returns a list without duplicate where the first occurance is kept.
/// </returns>
printfn "\n****** 4g2  ******"

let xs = [1;2;7;2;1;8;2]

let rec removeDuplicate = function
    |x::xs -> x::removeDuplicate(List.filter(fun y -> y<>x) xs)
    |_ -> []

printfn "%A" (removeDuplicate xs)

//Removing duplicates of empty lists
printfn "removeDuplicate test1: %b" (removeDuplicate [] = [])

//Removing duplicates of a list only with duplicates
printfn "removeDuplicate test2: %b" (removeDuplicate [1;1;1;1;1;1;1;1;1;1;1;1;1;] = [1])

//Removing duplicates of a list with no duplicates
printfn "removeDuplicate test3: %b" (removeDuplicate [1;2;3;4;5] = [1;2;3;4;5])

//Removing duplicates of a list with random elements
printfn "removeDuplicate test4: %b" (removeDuplicate [1;2;2;7;56;65;73;4;4;3] = [1;2;7;56;65;73;4;3])
