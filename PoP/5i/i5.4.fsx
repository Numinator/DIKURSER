let minFinder (rfA : 'a []) (index : int) =
 let mutable i = index
 let mutable min = (rfA.[i], i)
 while i + 1 < Array.length rfA do
  if fst min > rfA.[i + 1] then min <- (rfA.[i + 1], i + 1)
  i <- i + 1
 snd min

let arraySortD (arr : 'a []) =
 for i = 0 to Array.length arr - 1 do
   let tmp = arr.[i]
   let j = minFinder arr i
   arr.[i] <- arr.[j]
   arr.[j] <- tmp
 ()

let b = [|22;1;0|]
arraySortD b
printf "%A" ( b = [|0;1;22|])
