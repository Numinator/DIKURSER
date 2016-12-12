type Car(eff : float) = class
  let eff = eff
  let mutable fuel  = 0.0
  member this.GasLeft = fuel 
  member this.AddFuel (liters : float) = fuel <- fuel + liters
  member this.Drive (km : float) = 
    let distInFuel = km / eff
    if distInFuel > fuel then
      invalidArg "km" "You do not have enough fuel to drive that distance"
    else
      fuel <- fuel - distInFuel
end

type Moth(pos : float * float) = class
  let mutable pos = pos
  member this.GetPosistion : float*float = pos
  member this.MoveToLight (ligthCoor: float*float) =
    let x = fst pos
    let y = snd pos
    let xL = fst ligthCoor
    let yL = snd ligthCoor
    pos <- (x + (xL - x) / 2.0, y + (yL - y)/2.0)

end

let a = Moth((50.0, 10.0))
a.MoveToLight (10.0, -10.0)
printfn "%b" ((30.0, 0.0) = a.GetPosistion)



type Coord(x : float, y : float) = class
  let crds = (x, y)
  let length = sqrt (x*x + y*y)
  let unitVector = (x / length, y / length)
  member this.X = fst crds
  member this.Y = snd crds
  member this.GetLength () = length
  member this.GetUnitVector () = Coord(unitVector)
  static member (+) ((rhs : Coord), (lhs : Coord)) = 
    Coord(rhs.X + lhs.X, rhs.Y + lhs.Y)
  static member (-) ((rhs : Coord), (lhs : Coord)) = 
    Coord(rhs.X - lhs.X, rhs.Y - lhs.Y)
  static member (*) (a : float, C: Coord) =
    Coord(C.X * a, C.Y * a)
end

type Drone(pos: Coord, speed: float, dest: Coord) = class
  let mutable pos = pos
  let speed = speed * (dest - pos).GetUnitVector ()
  let dest = dest
  let mutable numTimes = int (pos.GetLength () / (dest - pos).GetLength ())
  member this.Fly () = 
    if numTimes < 0 then pos <- dest
    else 
      numTimes <- numTimes - 1
      pos <- pos + speed
end
