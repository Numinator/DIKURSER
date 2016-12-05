/// <summary>
///    Given a figure the function finds the bottom-left- and top-right
///    cornerpoint for the smallest "akserette" rectangle, which contains the
///    figure
/// </summary>
/// <remarks>
///    Input must be a figure of either type Circle, Rectangle, Mix, Twice
/// </remarks>
/// <param name="fig">
///    An figure
/// </param name="fig">
/// <returns>
///   Returns a tuple of two points, which are the bottom-left- and top-right
///    cornerpoint for the smallest "akserette" rectangle, which contains the
///    figure
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
    
(* Boundingbox *)
let rec boundingBox fig : point * point =
  let vectorize f (x0, y0) (x1, y1) = (f x0 x1, f y0 y1)
  match fig with
  | Circle    ((x, y), r, _) -> ((x-r, y-r) , (x+r, y+r))
  | Rectangle (p0, p1, _)    -> (p0, p1)
  | Mix (fig0, fig1)         ->
    let ((p0, q0), (p1, q1)) = (boundingBox fig0, boundingBox fig1)
    (vectorize min p0 p1, vectorize max q0 q1)
  | Twice (fig, (vx, vy))    ->
    let ((p0, q0), (p1, q1))=(boundingBox fig, boundingBox (move fig (vx,vy)))
    (vectorize min p0 p1, vectorize max q0 q1)
      
      

//Test
printfn "Bounding box test: %A" (boundingBox g61 = ((5,5), (145, 180)))
