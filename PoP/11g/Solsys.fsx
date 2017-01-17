let G = 1.0  // HUSK! at skifte G-værdien ud med den rigtige


type Vec3(x : double, y : double, z : double) = class
  let length = sqrt (x*x + y*y + z*z)
  member val X = x with get
  member val Y = y with get
  member val Z = z with get
  member this.GetLength () = length
  member this.GetUnitVector () = Vec3(x / length, y / length, z / length)
  override this.ToString() = "("+(string this.X)+", "+(string this.Y)+", "+(string this.Z)+")"
  static member (+) ((lhs : Vec3), (rhs : Vec3)) = 
    Vec3(lhs.X + rhs.X, lhs.Y + rhs.Y, lhs.Z + rhs.Z)
  static member (-) ((lhs : Vec3), (rhs : Vec3)) = 
    Vec3(lhs.X - rhs.X, lhs.Y - rhs.Y, lhs.Z - rhs.Z)
  static member (~-) (V : Vec3) = Vec3(-V.X, -V.Y, -V.Z)
  static member ( * ) (a : double, V: Vec3) =
    Vec3(V.X * a, V.Y * a, V.Z * a)
  static member ( * ) (V : Vec3, a: double) =
    Vec3(V.X * a, V.Y * a, V.Z * a)
  static member (/) (V : Vec3, a : double) =
    Vec3(V.X / a, V.Y / a, V.Z / a)
  static member DivideByInt (V : Vec3, i : int) =
     let a = float i
     Vec3(V.X / a, V.Y / a, V.Z / a)
  static member ( * ) (lhs : Vec3, rhs : Vec3) = //Cross product
    Vec3(lhs.Y * rhs.Z - lhs.Z * rhs.Y,
         lhs.Z * rhs.X - lhs.X * rhs.Z,
         lhs.X * rhs.Y - lhs.Y * rhs.X 
         )
  static member (|-|) (lhs : Vec3, rhs : Vec3) =
    (lhs - rhs).GetLength ()
end
// type Line(r : Vec3, p : Vec3) = class
//   member val R : Vec3 = r with get
//   member val P : Vec3 = p with get
//   static member ( ||? ) (lhs : Line, rhs : Line) = lhs.R * rhs.R = Vec3(0.0, 0.0, 0.0) //Are they Parallel?
//   static member ( =? ) (lhs : Line, rhs : Line) = //Are their point-set the same set
//     if (lhs ||? rhs) then
//       lhs.P.X / rhs.P.X = lhs.P.Y / rhs.P.Y && lhs.P.Y / rhs.P.Y = lhs.P.Z / rhs.P.Z
//     else false
//   // static member ( /? ) (lhs : Line, rhs : Line) = //Er de vindskave (OBS OVERSÆT TIL ENGELSK)
//   //   if (lhs ||? rhs) then
//   //     not (lhs.P.X / rhs.P.X = lhs.P.Y / rhs.P.Y && lhs.P.Y / rhs.P.Y = lhs.P.Z / rhs.P.Z)
//   //   else false
//   // static member ( / ) (lhs : Line, rhs : Line) = 
  
//   // static member ( - ) (lhs: Line, rhs : Vec3) = 
    
//   // static member ( ** ) (lhs : Vec3, rhs : Vec3) = Line(lhs - rhs, lhs) //Line generated from 2 points in space
// end

// type Plane(s : Vec3, t : Vec3, p : Vec3) = class
//   member val S = s with get
//   member val T = t with get
//   member val P = p with get
//   static member ( |p| ) (L : Line, Orient : Vec3, Planet : Vec3) =
    
    
// end

type IDFactory() = class
  let mutable ID : int = -1
  member this.GetNewID () = 
    ID <- ID + 1
    if ID < 0 then
      invalidArg "ID" "IDFactory deprecated, no more ID's to give"
    ID   
end

type Mass(r : double, m : double, pos : Vec3, initalVel : Vec3) = class
  static let ID_Generator = new IDFactory()
  member val ID : int = ID_Generator.GetNewID () with get
  member val M = m with get
  member val R = r with get
  member val P = pos with get, set
  member val V = initalVel with get, set
  override this.ToString() = string this.ID
end

type LocalSystem(rootMass : Mass) = class
  static let dummy : LocalSystem = LocalSystem(Mass(0.0,0.0,Vec3(1.0,1.0,1.0),Vec3(1.0,1.0,1.0)))
  let mutable nextPos = new Vec3(0.0, 0.0, 0.0)
  let mutable nextVel = new Vec3(0.0, 0.0, 0.0)
  member val posList : Vec3 list = [rootMass.P] with get, set
  member val TS : uint64 = 1uL
  member val RM : Mass = rootMass with get //RootMass
  member val SL : LocalSystem list = [] with get, set
  // member this.SimulateStep (LSL' : LocalSystem list) =
  //  let LSL = List.filter (fun (x : LocalSystem) -> x.RM.ID <> this.RM.ID) LSL'
  //  if not <| List.isEmpty LSL then
  //   let FL = List.map (fun (x : LocalSystem) -> G * ((x.RM.M * this.RM.M)/((x.RM.P |-| this.RM.P)**2.0))) LSL //ForceList (in Newton)
  //   let vecFL = List.map2 (fun (x : LocalSystem) F -> F * (this.RM.P - x.RM.P).GetUnitVector ()) LSL FL
  //   let vecF = List.fold (+) (new Vec3(0.0, 0.0, 0.0)) vecFL
  //   let avgMPos = List.fold2 (fun s (x : LocalSystem) F -> s + F * x.RM.P) (Vec3(0.0, 0.0, 0.0)) LSL FL / List.sum FL 
    
  //   ()
  //  else
  //   ()
  member this.AddLocalSystem (locSys : LocalSystem) =
    this.SL <- locSys::this.SL 
  member this.SimulateStepNaive (LSL' : LocalSystem list)  (VF : Vec3) = //Bruger at parent er head af LSL'
   let LSL = List.append (List.filter (fun (x : LocalSystem) -> x.RM.ID <> this.RM.ID) LSL') this.SL
   let FL = List.map (fun (x : LocalSystem) -> G * ((x.RM.M * this.RM.M)/((x.RM.P |-| this.RM.P)**2.0))) LSL //ForceList (in Newton)
   let vecFL = List.map2 (fun (x : LocalSystem) F -> F * (this.RM.P - x.RM.P).GetUnitVector ()) LSL FL
   let vecF = List.fold (+) (new Vec3(0.0, 0.0, 0.0)) vecFL + VF
   let a = vecF / this.RM.M
   nextPos <- this.RM.P + this.RM.V * (double this.TS) + (a * ((double this.TS) ** 2.0)) * 0.5
   nextVel <- this.RM.V + a * double this.TS
   let newVF = vecF - List.head vecFL
   let newLSL' = this::(List.head LSL)::this.SL
   List.iter (fun (x : LocalSystem) -> x.SimulateStepNaive newLSL' newVF) this.SL
   ()
  member this.AssertUpdate () =
     this.RM.P <- nextPos
     this.RM.V <- nextVel
     this.posList <- List.append this.posList [this.RM.P]
     List.iter  (fun (x : LocalSystem) -> x.AssertUpdate ()) this.SL
     ()
  member this.Simulate (n : int) =
    for i = 1 to n do 
      this.SimulateStepNaive [dummy] (Vec3(0.0, 0.0, 0.0))
      this.AssertUpdate ()
    ()
  member this.GetPosList () =
    if List.isEmpty this.SL then
      [(this.RM, this.posList)]
    else
      (this.RM, this.posList)::(List.collect (fun (x : LocalSystem) -> x.GetPosList ()) this.SL)
  
  override this.ToString() = string this.RM
    
  
    
end

let a = Mass(1.0, 10.0, Vec3(0.0, 0.0, 0.0), Vec3 (1.0, 0.0, 0.0))
let b = LocalSystem(a)

b.AddLocalSystem (LocalSystem(Mass(1.0, 10.0, Vec3(10.0, 0.0, 0.0), Vec3 (-1.0, 0.0, 0.0))))

b.Simulate 10000

printfn "%A" (b.GetPosList ())



