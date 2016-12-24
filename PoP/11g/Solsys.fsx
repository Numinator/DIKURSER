let G = 0.0


type Vec3(x : float, y : float, z : float) = class
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
  static member (*) (a : float, V: Vec3) =
    Vec3(V.X * a, V.Y * a, V.Z * a)
  static member (*) (V : Vec3, a: float) =
    Vec3(V.X * a, V.Y * a, V.Z * a)
//   static member (^) (V: Vec3) = Vec3(-V.Y, V.X)
end

// type Matrix(row:int, col:int, xs:'a []) = class
//   let m2d = Array2D.init row col (fun i j -> xs.[i + row * j])
//   new(x:'a, y:'a, z:'a) = Matrix(1, 3, [|x,y,z|])   
//   member val N = row with get
//   member val M = col with get
//   static member (*) (lhs : Matrix, rhs : Matrix) =
//     if 

// end


type Mass(m : float, initalImpuls : float) = class
  let mutable impuls = initalImpuls

  
end

type LocalSystem(rootMass : Mass) = class
  member x.RootMass : Mass = rootMass
  member val SystemList : (LocalSystem * Vec3) list = [] with get, set
  
end