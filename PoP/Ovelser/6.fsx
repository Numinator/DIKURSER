type weekday = Firstday = 0 | Nextday = 1

let dayToNumber = function
| Firstday -> 1
| Nextday  -> 2

let nextDay = function
| Firstday -> Nextday
| Nextday  -> Firstday

type point = int * int
type colour = int * int * int
type figure =
      | Circle of point * int * colour
      | Rectangle of point * point * colour
      | Mix of figure * figure

let rec draw (x, y) (fig : figure) =
  match fig with
  | Circle ((cx, cy), r, col) ->
      if (x-cx) * (x-cx) + (y-cy) * (y-cy) <= r*r
      then Some col else None
  | Rectangle ((x0, y0), (x1, y1), col) ->
      if x0 <= x && x <= x1 && y0 <= y && y <= y1
      then Some col else None
  | Mix (f1, f2) ->
      match (draw (x, y) f1, draw (x, y) f2) with
      | (None, c) -> c
      | (c, None) -> c
      | (Some (r1, g1, b1), Some (r2, g2, b2)) ->
          Some ((r1+r2)/2, (g1+g2/2), (b1+b2)/2)


let RED = (255, 0, 0)
let BLUE = (0, 0, 255)
let GRAY = (128, 128, 128)


let o61 = Mix (Circle ((50, 50), 45, RED), Rectangle((40, 40),(90, 110), BLUE))



let makePicture filnavn figur b h =
  let mPFeed x y =
    match draw (x, y) figur  with
    | None -> GRAY
    | _    -> draw (x, y) figur
  makeBMP filnavn b h mPFeed

let rec checkFigure fig =
  let  isColour (x, y, z) =
    match (x, y, z) with
    | (x, y, z) when max (max x y) z > 255 -> false
    | (x, y, z) when min (min x y) z < 0   -> false
    | _                                    -> true
  match fig with
  | Circle (_, r, col) -> if r > 0 && isColour col then true else false
  | Rectangle ((x0, y0), (x1, y1), col) -> if (x1 - x0) >= 0 && (y1 - y0) >= 0
                                              && isColour col then true
                                              else false
  | Mix (f1, f2) -> checkFigure f1 && checkFigure f2

let rec move (fig : figure) (vx, vy) =
  match fig with
  | Circle ((x, y), r, col)             -> ((x + vx, y + vy), r, col)
  | Rectangle ((x0, y0), (x1, y1), col) -> ((x0 + vx, y0 + vy),
                                            (x2 + vx, y1 + vy), col)
  | Mix (f1, f2) -> (move f1 (vx, vy), move f2 (vx, vy))

let rec boundingBox (fig : figure) =
  let bd = boundingBox
  let cmp op i1 i2 obj1 obj2 = op (i1 (i2 (bd obj1)) (i1 (i2 (bd obj2)))
  match fig with
  | Circle ((x, y), r, _)   -> ((x + r, y + r), (x - r, y - r))
  | Rectangle ((a), (b), _) -> ((a), (b))
  | Mix (f1, f2) ->  ((cmp min fst fst f1 f2, cmp min fst snd f1 f2),
                      (cmp max snd fst f1 f2, cmp max snd snd f1 f2))
