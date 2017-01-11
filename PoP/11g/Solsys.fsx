let G = 0.0  // HUSK! at skifte G-værdien ud med den rigtige


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
  static member DivideByInt (V : Vec3, i : int) =
     let a = float i
     Vec3(V.X / a, V.Y / a, V.Z / a)
  static member (|-|) (lhs : Vec3, rhs : Vec3) =
    (lhs - rhs).GetLength ()
  
end

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
  let mutable posList : Vec3 list = [rootMass.P]
  let mutable nextPos = new Vec3(0.0, 0.0, 0.0)
  let mutable nextVel = new Vec3(0.0, 0.0, 0.0)
  member val TS : uint64 = 0uL
  member val RM : Mass = rootMass with get //RootMass
  member val SL : LocalSystem list = [] with get
  member this.SimulateStep (LSL' : LocalSystem list) =
   let LSL = List.filter (fun (x : LocalSystem) -> x.RM.ID <> this.RM.ID) LSL'
   if not <| List.isEmpty LSL then
    let FL = List.map (fun (x : LocalSystem) -> G * ((x.RM.M * this.RM.M)/((x.RM.P |-| this.RM.P)**2.0))) LSL //ForceList (in Newton)
    let vecFL = List.map2 (fun (x : LocalSystem) F -> F * (this.RM.P - x.RM.P).GetUnitVector ()) LSL FL
    let vecF = List.fold (+) (new Vec3(0.0, 0.0, 0.0)) vecFL
    let avgMPos = List.fold2 (fun s (x : LocalSystem) F -> s + F * x.RM.P) (new Vec3(0.0, 0.0, 0.0)) LSL FL / List.sum FL 
    
    ()
   else
    ()
  member this.SimulateStepNaive (LSL' : LocalSystem list, VF : Vec3) = //Bruger at parent er head af LSL'
   let LSL = List.append <| List.filter (fun (x : LocalSystem) -> x.RM.ID <> this.RM.ID) LSL' <| this.SL
   if not <| List.isEmpty LSL then
    let FL = List.map (fun (x : LocalSystem) -> G * ((x.RM.M * this.RM.M)/((x.RM.P |-| this.RM.P)**2.0))) LSL //ForceList (in Newton)
    let vecFL = List.map2 (fun (x : LocalSystem) F -> F * (this.RM.P - x.RM.P).GetUnitVector ()) LSL FL
    let vecF = List.fold (+) (new Vec3(0.0, 0.0, 0.0)) vecFL + VF
    let a = vecF / this.RM.M
    nextPos <- this.RM.P + this.RM.V * double this.TS + (a * double this.TS ** 2.0) / 2.0
    nextVel <- this.RM.V + a * double this.TS
    let newVF = vecF - List.head vecFL
    let newLSL' = this::(List.head LSL)::this.SL
    List.iter (fun (x : LocalSystem) -> x.SimulateStepNaive(newLSL', newVF)) this.SL
   ()
   member this.AssertUpdate () =
     this.RM.P <- nextPos
     this.RM.V <- nextVel
     ()    
     
end