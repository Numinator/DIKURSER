/// <summary>
///   Finds the colour at one point and matches the point with a point
///   in a figure
/// </summary>
/// <remarks>
///   Input must be a point and a figure
/// </remarks>
/// <param name="(x,y)">
///    A point that takes a tuple of two integers
/// </param name="(x,y)">
/// <param name="figure">
///    A figure which is either a Circle, Rectangle, Mix, Twice
/// </param name="figure">
/// <returns>
///   Returns Some col if the inputted point matches a point in the figure.
///   Returns None if the above is false
/// </returns>

#r "./makeBMP"
open makeBMP

(* all the necessary types  *)
type point = int * int 
type colour = int * int * int 
type figure =
    | Circle of point * int * colour
    | Rectangle of point * point * colour
    | Mix of figure * figure
    | Twice of figure * (int * int)

(* Moves a figure with a vector *)
let rec move fig vect =
  let (|+|) (x0, y0) (x1, y1) = (x0 + x1, y0 + y1) // vector addition
  match fig with
    | Circle (p, r, c)      -> Circle (p |+| vect, r, c)
    | Rectangle (p0, p1, c) -> Rectangle (p0 |+| vect, p1 |+| vect, c)
    | Mix (fig0, fig1)      -> Mix (move fig0 vect, move fig1 vect)
    | Twice (fig, (vx,vy))  -> Twice (move fig vect, (vx,vy))

(* Doing an extension of coulorAt to work with type Twice *)
let rec colourAt ((x,y) : point) (figure : figure) =
  match figure with
    | Circle ((cx,cy), r, col) ->
      if (x-cx)*(x-cx)+(y-cy)*(y-cy) <= r*r
      then Some col else None
    | Rectangle ((x0,y0), (x1,y1), col) ->
      if x0<=x && x <= x1 && y0 <= y && y <= y1
      then Some col else None
    | Mix (f1, f2) ->
      match (colourAt (x,y) f1, colourAt (x,y) f2) with
      | (None, c) -> c
      | (c, None) -> c
      | (Some (r1,g1,b1), Some (r2,g2,b2)) ->
      Some ((r1+r2)/2, (g1+g2)/2, (b1+b2)/2)
    | Twice (fig, (vx, vy)) ->
      match  (colourAt (x,y) fig, colourAt (x,y) (move fig (vx,vy))) with
      | (None, c) -> c //Farven af den ene figur
      | (c, None) -> c //Farven af den anden figur
      | (c, cv) -> cv    //Ved overlap, farven af figur med vektor
