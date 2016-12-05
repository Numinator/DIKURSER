/// <summary>
///    Based on a filename, figure, width and height it creates a picture of 
///    the figure named after name  with the dimensions width and height
/// </summary>
/// <remarks>
///   Input must be a string, valid figure, and two integers
/// </remarks>
/// <param name="name">
///    A string
/// </param name="name">
/// <param name="figure">
///    A figure
/// </param name="figure">
/// <param name="width">
///    An integer
/// </param name="width">
/// <param name="height">
///    An integer
/// </param name="height">
/// <returns>
///   Returns a "name".BMP file which contains a picture of the figure with
///   with dimensions width and height
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
    
(* Declaring some colours *)
let RED  = (255, 0, 0)     : colour
let BLUE = (0, 0, 255)     : colour
let GREY = (128, 128, 128) : colour

(* Figure o61 from ø6.3  *)
let o61 = Mix (Circle ((50,50), 45, RED),
               Rectangle ((40,40), (90,110), BLUE)
               )
(* Declaring figure g61 of type twice based on o61  *)
let g61 = Twice (o61, (50,70))

(* Moves a figure with a vector *)
let rec move fig vect =
  let (|+|) (x0, y0) (x1, y1) = (x0 + x1, y0 + y1) // vector addition
  match fig with
    | Circle (p, r, c)      -> Circle (p |+| vect, r, c)
    | Rectangle (p0, p1, c) -> Rectangle (p0 |+| vect, p1 |+| vect, c)
    | Mix (fig0, fig1)      -> Mix (move fig0 vect, move fig1 vect)
    | Twice (fig, (vx,vy))  -> Twice (move fig vect, (vx,vy))

(* Finds the colour at one point and matches the point with a point in a figure *)
let rec colourAt ((x,y) : point) (figure : figure) =
  match figure with
    | Circle ((cx,cy), r, col) ->
      if (x-cx)*(x-cx)+(y-cy)*(y-cy) <= r*r
      then Some col else None
    | Rectangle ((x0,y0), (x1,y1), col) ->
      if x0<=x && x <= x1 && y0 <= y && y <= y1 // indenfor hjørnerne
      then Some col else None
    | Mix (f1, f2) ->
      match (colourAt (x,y) f1, colourAt (x,y) f2) with
      | (None, c) -> c // overlapper ikke
      | (c, None) -> c // ditto
      | (Some (r1,g1,b1), Some (r2,g2,b2)) ->
      Some ((r1+r2)/2, (g1+g2)/2, (b1+b2)/2) // gennemsnitsfarve
    | Twice (fig, (vx, vy)) ->
      match  (colourAt (x,y) fig, colourAt (x,y) (move fig (vx,vy))) with
      | (None, c) -> c
      | (c, None) -> c
      | (c, k) -> k

(* Creates a picture *)
let makePicture name figure width height =
  let f point =
    match colourAt point figure with
      | Some colour -> colour
      | None        -> GREY
  makeBMP name width height f
  
makePicture "g63" g61 150 200
