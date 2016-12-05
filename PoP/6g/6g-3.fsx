type point = int * int
type colour = int * int * int
type figure =
      | Circle of point * int * colour
      | Rectangle of point * point * colour
      | Mix of figure * figure
      | Twice of figure * (int * int)

let RED = (255, 0, 0)
let BLUE = (0, 0, 255)
let GRAY = (128, 128, 128)

let rec colourAt (x, y) (fig : figure) =
  match fig with
  | Circle ((cx, cy), r, col) ->
      if (x-cx) * (x-cx) + (y-cy) * (y-cy) <= r*r
      then Some col else None
  | Rectangle ((x0, y0), (x1, y1), col) ->
      if x0 <= x && x <= x1 && y0 <= y && y <= y1
      then Some col else None
  | Mix (f1, f2) ->
      match (colourAt (x, y) f1, colourAt (x, y) f2) with
      | (None, c) -> c
      | (c, None) -> c
      | (Some (r1, g1, b1), Some (r2, g2, b2)) ->
          Some ((r1+r2)/2, (g1+g2/2), (b1+b2)/2)
  | Twice (f, (vx, vy)) ->
      match (colourAt (x, y) f, colourAt (x + vx, y + vy) f)
      | (None, c) -> c
      | (c, None) -> c
      | (_ , c)   -> c
