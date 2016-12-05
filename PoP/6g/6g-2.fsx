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
      

let o61 = Mix (Circle ((50, 50), 45, RED), Rectangle((40, 40),(90, 110), BLUE))

let g61 = Twice (o61, (50, 70))
