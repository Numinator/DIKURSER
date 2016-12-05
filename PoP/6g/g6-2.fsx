/// <summary>
///   Creates a figure of the type Twice based on the figure o61 and the
///   the vector (50,70)
/// </summary>
/// <remarks>
///   Input must be a figure and a vector with (int * int)
/// </remarks>
/// <param name="">
///    
/// </param name="">
/// <returns>
///   Returns a figure of the type Twice. o61 and its copy moved with
///   vector (50, 70) is basically returned
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
