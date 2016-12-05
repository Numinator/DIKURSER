/// <summary>
///    Converts the numbers 1-7 to its equivalent weekday
/// </summary>
/// <remarks>
///   input must be an integer
/// </remarks>
/// <param name="n">
///    An integer
/// </param name="n">
/// <returns>
///   Returns None if the inputted integer is anything but 1-7
/// </returns>
type weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

(* converts the weekday to its equivalent number *)
let dayToNumber day =
    match day with
    |Monday -> 1
    |Tuesday -> 2
    |Wednesday -> 3
    |Thursday -> 4
    |Friday -> 5
    |Saturday -> 6
    |Sunday -> 7
    
(* numberToDay  *)
let numberToDay (n: int) =
    match n with
    |_ when n < 0 || 7 < n -> None
    |1 -> Some Monday
    |2 -> Some Tuesday
    |3 -> Some Wednesday
    |4 -> Some Thursday
    |5 -> Some Friday
    |6 -> Some Saturday
    |7 -> Some Sunday

printfn "%A" (numberToDay (dayToNumber Monday))

//Converting a negative integer to a weekday
printfn "numberToDay test1: %b" (numberToDay -1 = None )

//Converting a number greater than 7
printfn "numberToDay test2: %b" (numberToDay 1023111 = None )

//Converting a number to a number between 1-7
printfn "numberToDay test2: %b" (numberToDay 2 =Some Tuesday )

