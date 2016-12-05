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
///   Returns an intergerut list.
/// </returns>
let rec count = function
    |(x::xs, x') -> (if x = x' then 1 else 0) + count(xs,x')
    |_          -> 0

printfn "%i" (count([1;2;2;3;4;4;5],4))

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
    |(x::xs, x') -> x'::x::xs
    |(_,x') -> [x']

printfn "%A" (insert ([1;2;3],4))

printfn "%A" (insert ([1;1;4;6;7],5))

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
    | (x::xs, y::ys) when x = y -> x::intersect(xs,ys)
    | (x::xs, y::ys) when x < y -> intersect(xs,y::ys)
    | (x::xs, y::ys)            -> intersect(x::xs,ys)
    |_ -> []

printfn "%A" (intersect ([1;1;1;2;2],[1;1;2;4]))


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
    |(x::xs,[])                    -> x::xs
    |([],x'::xs')                  -> x'::xs'
    |(x::xs, x'::xs') when x <= x' -> x::plus(xs,x'::xs')
    |(x::xs, x'::xs')              -> x'::plus(x::xs,xs')
    |_                             -> []

printfn "%A" (plus([1;1;2],[1;2;4]))


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
    |(x::xs,x'::xs') when x = x' -> minus(xs,xs')
    |(x::xs,x'::xs') when x < x' -> x::minus(xs,xs')
    |(x::xs,x'::xs') when x > x' -> minus(x::xs,xs')
    |(x::xs,[]) -> x::xs
    |([],x'::xs') -> x'::xs'
    |_-> []

printfn "%A" (minus([1;1;1;2;2],[1;1;2;3]))

printfn "%A" (minus([1;1;2;3],[1;1;1;2;2]))



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

/// <summary>
///   Removes duplicates of elements in a list, and keeps the first occurance.
/// </summary>
/// <param name="l">
///   a list.
/// </param name="l">
/// <returns>
///   Returns a list without duplicate where the first occurance is kept.
/// </returns>

let xs = [1;2;7;2;1;8;2]

let rec removeDuplicate = function
    |x::xs -> x::removeDuplicate(List.filter(fun y -> y<>x) xs)
    |_ -> []

printfn "%A" (removeDuplicate xs)
