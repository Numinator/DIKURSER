open System.Drawing


let load s = 
  // Reads the JPG image into a bitmap
  let bmp = new Bitmap(Image.FromFile(s))
  let get50Shades x y = 
    let c = bmp.GetPixel (x, y)
    int(c.R + c.G + c.B) / 3
  [|for y in 0 .. bmp.Height-1 do for x in 0 .. bmp.Width-1 -> get50Shades x y|]

let putInBucket bucketSizeWish bmp =
  let size = 255 / bucketSizeWish
  let bucket = Array.init size (fun _ -> 0)
  



load "coins.jpg"