open System
open System.Drawing
open System.Windows.Forms

let load s = 
  // Reads the JPG image into a bitmap
  let bmp = new Bitmap(Image.FromFile(s))
  let get255Shades x y = // of gray
    let c = bmp.GetPixel (x, y)
    int(c.R + c.G + c.B) / 3
  [|for y in 0 .. bmp.Height-1 do for x in 0 .. bmp.Width-1 -> get255Shades x y|]

let putInBuckets numBucketWish bmp =
  if numBucketWish < 1 && numBucketWish > 256 then
    invalidArg "numBucketWish" "Wish for number of buckets must be between 1 and 256, inclusive"
  let rec findBuckets n = if 256 % n = 0 then n else findBuckets (n - 1)
  let size = findBuckets numBucketWish 
  let buckets = Array.init size (fun _ -> 0)
  Array.iter (fun n -> buckets.[(n / 256)*size] <- buckets.[(n / 256)*size] + 1) bmp
  buckets

let drawHistogram 



  



load "coins.jpg"