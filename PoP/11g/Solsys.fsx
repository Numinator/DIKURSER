let G = 6.674083100 * (10.0 ** (-20.0))  // HUSK! at skifte G-værdien ud med den rigtige

/// ===========================================
/// Vector with 3 coordinates
/// ===========================================
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


/// ===========================================
/// ID Factory
/// ===========================================

type IDFactory() = class
  let mutable ID : int = -1
  member this.GetNewID () = 
    ID <- ID + 1
    if ID < 0 then
      invalidArg "ID" "IDFactory deprecated, no more ID's to give"
    ID   
end

/// ===========================================
/// Mass data collection
/// ===========================================

type Mass(r : double, m : double, pos : Vec3, initalVel : Vec3) = class
  static let ID_Generator = new IDFactory()
  member val ID : int = ID_Generator.GetNewID () with get
  member val M = m with get
  member val R = r with get
  member val P = pos with get, set
  member val V = initalVel with get, set
  override this.ToString() = string this.ID
end


/// ===========================================
/// Class that acts on the Mass data collection
/// ===========================================

type LocalSystem(rootMass : Mass) = class
  let mutable nextPos = new Vec3(0.0, 0.0, 0.0)
  let mutable nextVel = new Vec3(0.0, 0.0, 0.0)
  member val posList : Vec3 list = [] with get, set
  member val TS : uint64 = 1uL // Time Step
  member val RM : Mass = rootMass with get // Root Mass
  member val SL : LocalSystem list = [] with get, set
  member this.AddLocalSystem (locSys : LocalSystem) =
    this.SL <- locSys::this.SL 
  member this.SimulateStepNaive (LSL' : LocalSystem list)  (VF : Vec3) = //Uses/Depends on the fact that the parent is in the head of LSL'
   // Makes a list of planets that it shold simualte its attraction to (a LocalSystem List -> LSL)
   let LSL = List.append (List.filter (fun (x : LocalSystem) -> x.RM.ID <> this.RM.ID) LSL') this.SL

   // Calculates the magnetude of the force exerted by each mass in LSL (Force List -> FL)
   let FL = List.map (fun (x : LocalSystem) -> G * ((x.RM.M * this.RM.M)/((x.RM.P |-| this.RM.P)**2.0))) LSL

   // Calculates the force vector exerted by each mass given a magnetude and unitvector of the relatice position (vector Force List -> vecFL)
   let vecFL = List.map2 (fun (x : LocalSystem) F -> F * (this.RM.P - x.RM.P).GetUnitVector ()) LSL FL

   // Calculates the sum of the vector forces plus the sum of the vector forces exerted on the parent (though not the force this planet exerts on the parent)
   let vecF = List.fold (+) (new Vec3(0.0, 0.0, 0.0)) vecFL + VF - List.head vecFL

   // Calculates the acceleration vector 
   let a = vecF / this.RM.M

   // Calculates the next position and velocity
   nextPos <- this.RM.P + this.RM.V * (double this.TS) + (a * ((double this.TS) ** 2.0)) * 0.5
   nextVel <- this.RM.V + a * double this.TS

   // Calculates new VF and LSL' and recursively calls SimulateStepNaive on childs
   let newVF = vecF - List.head vecFL
   let newLSL' = this::(List.head LSL)::this.SL
   List.iter (fun (x : LocalSystem) -> x.SimulateStepNaive newLSL' newVF) this.SL
   ()
  member this.AssertUpdate () =
     this.RM.P <- nextPos
     this.RM.V <- nextVel
     this.posList <- List.append this.posList [this.RM.P] // This line make Simulate > theta (n) :-(
     List.iter  (fun (x : LocalSystem) -> x.AssertUpdate ()) this.SL
     ()
  member this.Simulate (n : int) =
    for i = 1 to n do 
      this.SimulateStepNaive [] (Vec3(0.0, 0.0, 0.0))
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

b.AddLocalSystem (LocalSystem(Mass(1.0, 10.0, Vec3(10.0, 0.0, 0.0), Vec3 (-1.0, 0.0, 10.0))))
b.AddLocalSystem (LocalSystem(Mass(1.0, 10.0, Vec3(10.0, 0.0, 10.0), Vec3 (1.0, 01.0, 0.0))))
b.AddLocalSystem (LocalSystem(Mass(1.0, 10.0, Vec3(10.0, 10.0, 0.0), Vec3 (-1.0, 110.0, 0.0))))
b.AddLocalSystem (LocalSystem(Mass(1.0, 10.0, Vec3(10.0, 022.0, 0.0), Vec3 (-1.0, 110.0, 0.0))))
b.AddLocalSystem (LocalSystem(Mass(1.0, 10.0, Vec3(10.0, 03.0, 220.0), Vec3 (-1.0, 20.20, 0.0))))
b.AddLocalSystem (LocalSystem(Mass(1.0, 10.0, Vec3(10.0, 4440.0, 0.0), Vec3 (-1.0, 0.0, 450.0))))
b.AddLocalSystem (LocalSystem(Mass(1.0, 10.0, Vec3(10.0, 01221.0, 22022.0), Vec3 (-1.0, 0.0, 20.0))))
b.AddLocalSystem (LocalSystem(Mass(1.0, 10.0, Vec3(10.0, 1110.0111, 0.0), Vec3 (-1.0, 0.660, 131230.0))))
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
b.Simulate 10

stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

printfn "%A" (b.GetPosList ())


