let rec isTable = function
 | [[_]]               -> true
 | [_] | []            -> false
 | x0::(x1::xs as xss) -> x0.length = x1.length & isTable xss
 | _                   -> false

let firstColumn n
 if isTable n then
  let rec resMaker = function
   | []            -> []
   | (x0::xs)::xss -> x0::(resMaker xss)  
