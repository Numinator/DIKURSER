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
  let mutable crds = (x, y)
  member this.X = fst crds
  member this.Y = snd crds
  member this.SetWithCoord (coord : Coord) =
    crds <- (coord.X, coord.Y)
  member this.SetWithFloats (floatTuple : float * float) =
    crds <- floatTuple
  static member (+) ((rhs : Coord), (lhs : Coord)) = 
    Coord(rhs.X + lhs.X, rhs.Y + lhs.Y)
  static member (-) ((rhs : Coord), (lhs : Coord)) = 
    Coord(rhs.X - lhs.X, rhs.Y - lhs.Y)
  static member GetLengt (c : coord) =
end

type Drone(pos: Coord, speed: Coord, dest: Coord) = class
  
  let mutable pos = pos
  let speed = speed
  let dest = dest
  
  member this.Fly () = 
    if (pos.X)



end
