/// <summary>
///    Given a figure the function finds figures out if the radius of a circle
///    is non negative, if the the cornerleft point of a rectangle is below
///    and to the left of the upperright cornerpoint. It also checks if the
///    colour components is between 0 and 255.
/// </summary>
/// <remarks>
///    Input must be a figure
/// </remarks>
/// <param name="fig">
///    An figure
/// </param name="fig">
/// <returns>
///    Given a circle it returns true, if the radius is non-negative and the
///    colourcomponents are correct
///    Given a rectangle it returns true, if the cornerleft point of rectangle
///    is below and to the left of the upperright cornerpoint and the colour
///    components are correct
///
/// </returns>#r "./makeBMP"
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

(* CheckFigure *)
let rec checkFigure fig =
  let byte    b        = 0 <= b && b <= 255
  let colour (r, g, b) = byte r && byte g && byte b
  match fig with
    | Circle    (_, r, c)             -> 0 <= r           && colour c
    | Rectangle ((x0,y0), (x1,y1), c) -> x0<=x1 && y0<=y1 && colour c
    | Mix (fig0, fig1)                -> checkFigure fig0 && checkFigure fig1
    | Twice (fig, (vx, vy))            ->
        checkFigure fig && checkFigure (move fig (vx,vy))

printfn "CheckFigure Illegal Circle Test:     %b" (checkFigure (Circle ((0,0), -1, RED)) = false )
printfn "CheckFigure Legal Circle Test:       %b" (checkFigure (Circle ((0,0), 2, RED)) = true )

printfn "CheckFigure Illegal Rectangle Test:  %b" (checkFigure (Rectangle ((-1,-1), (-1,1), RED)) = false)
printfn "CheckFigure Legal Rectangle Test:    %b" (checkFigure (Rectangle ((-1,-1), (1,1), RED)) = true)

printfn "CheckFigure Illegal Colour Test:     %b" (checkFigure (Rectangle ((-1,-1), (1,1), (300, 300, 300)) = false)
