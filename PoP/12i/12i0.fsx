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
  let rec findBuckets n = if 256 % n = 0 then n else findBuckets (n - 256 % n)
  let size = findBuckets numBucketWish 
  let buckets = Array.init size (fun _ -> 0)
  Array.iter (fun n -> buckets.[(n*size)/256] <- buckets.[(n*size)/256] + 1) bmp
  buckets

let drawHistogram s' p' (buckets : int []) =
  let win = new Form ()
  win.Text <- "Historgram with "+sprintf "%i" (Array.length buckets)+" buckets"
  win.BackColor  <- Color.White
  let (s,p) = (max s' 200, max p' 50) // s for square(i.e. x = y); p for padding
  win.ClientSize <- Size (s, s)
  let pen = new Pen (Color.Black)
  let frame = [|Point (p, p); Point (p, s - p); 
                Point (s - p, s - p); Point (s - p, p);
                Point (p, p)|]
  // Translate from frame coordinates to client and inverts y-axsis
  let f2C (x, y) = (x + p , s - (y + p))
  let xF2C x = x + p
  let yF2C y = s - (y + p)
  
  //Calculate width of collums
  let wCol   = (s - 2 * p) / Array.length buckets
  //Calculate function for height of collums
  let hMax   = Array.max buckets 
  let hCol n = (n * (s - 2 * p)) / hMax
  
  let brush = new SolidBrush (Color.DarkBlue)
  let drawCollum k n (e : PaintEventArgs) = // number k collum with frequency of n shades of gray 
    let x' = xF2C <| k * wCol
    let y' = yF2C <| hCol n
    let w' = wCol
    let h' = hCol n
    printfn "%i x %i" wCol (hCol n)
    e.Graphics.DrawRectangle (pen, x', y', w', h')


  let rec findIncrement n =
    let increments = [1, 2, 5]



  Array.iter2 (fun k n -> win.Paint.Add <| drawCollum k n) [|0.. Array.length buckets - 1|] buckets 

  win.Paint.Add (fun e -> e.Graphics.DrawLines (pen, frame))
  Application.Run win

    



  



load "coins.jpg" |> putInBuckets 256 |> drawHistogram 1000 50