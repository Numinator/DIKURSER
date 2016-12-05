module Figure

type point = int * int
type colour = int * int * int
type figure =
      | Circle of point * int * colour
      | Rectangle of point * point * colour
      | Mix of figure * figure
      | Twice of figure * (int * int)

let rec move (fig : figure) (vx, vy) =
  match fig with
  | Circle ((x, y), r, col)             -> ((x + vx, y + vy), r, col)
  | Rectangle ((x0, y0), (x1, y1), col) -> ((x0 + vx, y0 + vy),
                                            (x2 + vx, y1 + vy), col)
  | Mix (f1, f2) -> (move f1 (vx, vy), move f2 (vx, vy))
  | Twice (f, (v)) -> (move f (vx, vy), (v))


let rec checkFigure fig =
  let  isColour (x, y, z) =
    match (x, y, z) with
    | (x, y, z) when max (max x y) z > 255 -> false
    | (x, y, z) when min (min x y) z < 0   -> false
    | _                                    -> true
  match fig with
  | Circle (_, r, col) -> r > 0 && isColour col
  | Rectangle ((x0, y0), (x1, y1), col) -> (x1 - x0) >= 0 && (y1 - y0) >= 0
                                           && isColour col
  | Mix (f1, f2)       -> checkFigure f1 && checkFigure f2
  | Twice (f, _)       -> checkFigure f



let rec boundingBox (fig : figure) =
  let bd = boundingBox
  let cmp op i1 i2 obj1 obj2 = op (i1 (i2 (bd obj1)) (i1 (i2 (bd obj2)))
  match fig with
  | Circle ((x, y), r, _)   -> ((x + r, y + r), (x - r, y - r))
  | Rectangle ((a), (b), _) -> ((a), (b))
  | Mix (f1, f2) ->  ((cmp min fst fst f1 f2, cmp min fst snd f1 f2),
                      (cmp max snd fst f1 f2, cmp max snd snd f1 f2))
  | Twice (f1, (v)) ->
                        let f2 = move f1 (v)
                        ((cmp min fst fst f1 f2, cmp min fst snd f1 f2),
                         (cmp max snd fst f1 f2, cmp max snd snd f1 f2))
