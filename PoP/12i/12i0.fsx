open System
open System.Drawing
open System.Windows.Forms

let load s = 
  // Reads the JPG image into a bitmap
  let bmp = new Bitmap(Image.FromFile(s))
  let get255Shades x y = // of gray
    let c = bmp.GetPixel (x, y)
    int(c.R + c.G + c.B) / 3
  [|for y in 0 .. bmp.Height-1 do for x in 0.. bmp.Width-1 -> get255Shades x y|]

let putInBuckets numBucketWish bmp =
  if numBucketWish < 1 && numBucketWish > 256 then
    invalidArg "numBucketWish" "Wish for number of buckets must be between 1 and 256, inclusive"
  let rec findBuckets n = if 256 % n = 0 then n else findBuckets (n - 1)
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
  let brush = new SolidBrush (Color.Black)
  let frame = [|Point (p, p); Point (p, s - p); 
                Point (s - p, s - p); Point (s - p, p);
                Point (p, p)|]
  let letterSize = 30 //pixel
  // Translate from frame coordinates to client and inverts y-axsis
  let f2C (x, y) = (x + p , s - (y + p))
  let xF2C x = x + p
  let yF2C y = s - (y + p)
  
  //Calculate width of collums in pixel
  let length = Array.length buckets
  let wCol   = (s - 2 * p) / length
  //Calculate function for height of collums in pixel
  let hMax   = Array.max buckets 
  let hCol n = (n * (s - 2 * p)) / hMax
  
  let drawCollum k n (e : PaintEventArgs) = 
  // number k collum with frequency of n shades of gray 
    let x' = xF2C <| k * wCol
    let y' = yF2C <| hCol n
    let w' = wCol
    let h' = hCol n
    e.Graphics.DrawRectangle (pen, x', y', w', h')

  // has structure (pixel dist, (incrementor, exponent))
  let findIncrementY =
    let firstDigits : int list = [1; 2; 5] //Must be sorted in non-decreasing
    let minDist = double (letterSize * hMax) / double (s - 2 * p)

    let findExp (d : double) =
      if d < 0.0 then invalidArg "d" "Findexp does not support negative numbers" 
      let rec findExp' n e = if n > 9 then findExp' (n / 10) (e + 1) else e
      if d < 1.0 then
        -(findExp' (int (d ** -1.0)) 0)
      else
        findExp' (int d) 0
    
    let exp = findExp minDist
    let minFD = int (ceil (minDist / (10.0 ** (double exp))))
    let chooseFirstDigit = 
      try 
        let a = List.find (fun x -> minFD <= x) firstDigits
        (a, exp)
      with
      | _ -> (List.head firstDigits, exp + 1)
    let FD = (double (fst chooseFirstDigit)) * 
             (10.0 ** (double (snd chooseFirstDigit))) 
    let distInPixel = (double (s - 2 * p) * FD) / double hMax
    (distInPixel, chooseFirstDigit)
  
  let drawYNotch k (e : PaintEventArgs) = // k'th notch 
    let dist = fst findIncrementY

    let y  = yF2C <| k * int dist
    let x  = xF2C <| 0
    let x' = xF2C <| -5
    let points = [|Point (x ,y); Point (x', y)|]

    e.Graphics.DrawLines (pen, points)
  
  let drawYText k (e : PaintEventArgs) =
    let dist = fst findIncrementY
    let incr = fst (snd findIncrementY)

    let text = sprintf "%i" (k * incr)
    let font = new Font ("Verdana", 16.0f)
    let x = float32 (xF2C <| -(p - 3))
    let y = float32 (yF2C <| k * (int dist) + letterSize / 2)

    e.Graphics.DrawString (text, font, brush, x, y)
  
  let drawExp (e : PaintEventArgs) =
    let exp = snd (snd findIncrementY)

    let text = sprintf "10e%i" exp
    let font = new Font ("Verdana", 16.0f)
    let x = float32 (xF2C <| 0)
    let y = float32 (yF2C <| (s - p) - (p - letterSize))

    e.Graphics.DrawString (text, font, brush, x, y)
  //Has structure (pixel dist, incrementor)
  let findIncrementX =
    let n = 50.0 // width of the text on the x-axis
    // how many markers can we cram in the x-axis
    // if they fill n pixel and they have to be a nice number?
    let niceIncrNumbers = [for i in 0 .. 8 -> 2.0 ** (double i)]
    let valPerPixel = 256.0 / double (wCol * length)
    let minMarkerLen = n * valPerPixel
    let valIncrLen = List.find (fun x -> minMarkerLen <= x) niceIncrNumbers
    let pixPerIncr = valIncrLen / valPerPixel
    (pixPerIncr, valIncrLen)

  let drawXNotch k (e : PaintEventArgs) =
    let dist = fst findIncrementX

    let x  = xF2C <| k * int dist
    let y  = yF2C <| 0
    let y' = yF2C <| -5
    let points = [|Point(x, y); Point(x , y')|]

    e.Graphics.DrawLines (pen, points)
  
  let drawXText k (e : PaintEventArgs) =
    let dist = fst findIncrementX
    let incr = snd findIncrementX
    let text = sprintf "%i" (k * incr)
    let font = new Font ("Verdana", 16.0f)
    let x = float32 (xF2C <| k * int dist - 50 / 2)
    let y = float32 (yF2C <| -8)
    e.Graphics.DrawString (text, font, brush, x, y)
  
   
  Array.iter (fun k ->
              win.Paint.Add <| drawYNotch k
              win.Paint.Add <| drawYText  k
              ) [|0 .. (s - 2 * p) / int (fst findIncrementY)|] 

  Array.iter (fun k ->
              win.Paint.Add <| drawXNotch k
              win.Paint.Add <| drawXText  k
              ) [|0 .. (s - 2 * p) / int (fst findIncrementX)|] 

              

  Array.iter2 (fun k n -> 
               win.Paint.Add <| drawCollum k n) [|0.. length - 1|] buckets 
  win.Paint.Add drawExp
  win.Paint.Add (fun e -> e.Graphics.DrawLines (pen, frame))
  Application.Run win

    



  



load "coins.jpg" |> putInBuckets 64 |> drawHistogram 1000 50