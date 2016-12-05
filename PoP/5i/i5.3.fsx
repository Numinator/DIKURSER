

let rec arraySort arr =
  let mid = Array.length arr / 2
  if mid = 0 then
    arr
  else
    let com1 y = arr.[mid] <= y
    let com2 y = arr.[mid] > y
    let lhs = arraySort (Array.filter com1 (Array.append arr.[..mid-1] arr.[mid+1..]))
    let rhs = arraySort (Array.filter com2 (Array.append arr.[..mid-1] arr.[mid+1..]))
    Array.append lhs (Array.append [|arr.[mid]|] rhs)
arraySort [|1; 3; 2; 4; 2|]
