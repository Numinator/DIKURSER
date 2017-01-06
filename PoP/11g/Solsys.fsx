let G = 0.0


type Vec3(x : double, y : double, z : double) = class
  let length = sqrt (x*x + y*y + z*z)
  member val X = x with get
  member val Y = y with get
  member val Z = z with get
  member this.GetLength () = length
  member this.GetUnitVector () = Vec3(x / length, y / length, z / length)
  static member (+) ((lhs : Vec3), (rhs : Vec3)) = 
    Vec3(lhs.X + rhs.X, lhs.Y + rhs.Y, lhs.Z + rhs.Z)
  static member (-) ((lhs : Vec3), (rhs : Vec3)) = 
    Vec3(lhs.X - rhs.X, lhs.Y - rhs.Y, lhs.Z - rhs.Z)
  static member (~-) (V : Vec3) = Vec3(-V.X, -V.Y, -V.Z)
  static member (*) (a : double, V: Vec3) =
    Vec3(V.X * a, V.Y * a, V.Z * a)
  static member (*) (V : Vec3, a: double) =
    Vec3(V.X * a, V.Y * a, V.Z * a)
  static member (/) (V : Vec3, a : double) =
    Vec3(V.X / a, V.Y / a, V.Z / a)
end

// type Matrix(row:int, col:int, xs:'a []) = class
//   let m2d = Array2D.init row col (fun i j -> xs.[i + row * j])
//   new(x:'a, y:'a, z:'a) = Matrix(1, 3, [|x,y,z|])   
//   member val N = row with get
//   member val M = col with get
//   static member (*) (lhs : Matrix, rhs : Matrix) =
//     if 

// end
type IDFactory() = class
  let mutable ID : int = -1
  member this.GetNewID () = 
    ID <- ID + 1
    if ID < 0 then
      invalidArg "ID" "IDFactory deprecated, no more ID's to give"
    ID   
end

type Mass(r : double,m : double, pos : Vec3, initalVel : Vec3) = class
  static let ID_Generator = new IDFactory()
  member val ID : int = ID_Generator.GetNewID () with get
  member val M = m with get
  member val R = r with get
  member val P = pos with get, set
  member val V = initalVel with get, set
end

type LocalSystem(rootMass : Mass) = class
  member this.RootMass : Mass = rootMass
  member val SystemList : LocalSystem list = [] with get, set
  member this.SimulateStep (LSL : LocalSystem list) =
    List.filter (fun x -> x.RootMass.ID <> this.RootMass.ID) LSL
    
     
end