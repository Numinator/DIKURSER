open System.IO
open System.Text.RegularExpressions
open System.Windows.Forms
open System.Drawing

let G : double = 6.674083100 * (10.0 ** (-20.0))  // km - kg - s



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
  member val Name = "No Name" with get, set
  override this.ToString() = this.Name
end


/// ===========================================
/// Class that acts on the Mass data collection
/// ===========================================

type LocalSystem(rootMass : Mass) = class
  let mutable nextPos = new Vec3(0.0, 0.0, 0.0)
  let mutable nextVel = new Vec3(0.0, 0.0, 0.0)
  member val TS : uint64 = 1uL with get, set // Time Step
  member val RM : Mass = rootMass with get // Root Mass
  member val SL : LocalSystem list = [] with get, set

  member val posList : Vec3 list = [rootMass.P] with get, set
  member this.AddLocalSystem (locSys : LocalSystem) =
    this.SL <- locSys::this.SL 
  member this.SimulateStepNaive (LSL' : LocalSystem list) = //Uses/Depends on the fact that the parent is in the head of LSL'
   
  member this.AssertUpdate () =
     this.RM.P <- nextPos
     this.RM.V <- nextVel
     this.posList <- List.append this.posList [this.RM.P] // This line make Simulate > theta (n) :-(
     List.iter  (fun (x : LocalSystem) -> x.AssertUpdate ()) this.SL
     ()
  member this.SimulateSol (n : int) =
    for i = 1 to n do 
      this.SimulateStepNaive []
      this.AssertUpdate ()
    ()
  member this.GetPosList () : (Mass * Vec3 list) list =
    if List.isEmpty this.SL then
      [(this.RM, this.posList)]
    else
      (this.RM, this.posList)::(List.collect (fun (x : LocalSystem) -> x.GetPosList ()) this.SL)
  
  override this.ToString() = string this.RM
    
  
    
end

type ReadData (name:string) = class

    let filePath = name+".txt"

    let massRegex =
        "mass.*\d{2}\^(?<notation>\d{2}).*=.*(?<mass>[0-9]\.[0-9]*)"
    let radiusRegex =
        "(mean.*radius|radius.*pluto).*=\s*(?<radius>[0-9]*\.*[0-9]*)?.*[a-z]"

    let dataRegex = "(?<1>\-*\d*\.*\d*)?\s+(?<2>\-*\d*\.*\d*)?\s+\
                     (?<3>\-*\d*\.*\d*)?\s+(?<4>\-*\d*\.*\d*)?.*"

    let setMass():double =
        use streamReader = new StreamReader(filePath)
        let rec mass() =
            if not streamReader.EndOfStream then
                let line = streamReader.ReadLine().ToLower()
                let n = Regex.Match(line,massRegex)
                if n.Success then
                    (System.Double.Parse(n.Result("${mass}"))*10.0**
                     System.Double.Parse(n.Result("${notation}")))
                else mass()
            else failwith "The mass could not be found"
        mass()


    let setRadius() =
        use streamReader = new StreamReader(filePath)
        let rec radius() =
            if not streamReader.EndOfStream then
                let line = streamReader.ReadLine().ToLower()
                let n = Regex.Match(line,radiusRegex)
                if n.Success then
                    (System.Double.Parse(n.Result("${radius}")))
                else radius()
            else failwith "the radius could not be found"
        radius()

    let dataCollect():double list list =
        use sr = new StreamReader(filePath:string)
        let rec read2Exit (exitValue:string):double list list =
            let line = sr.ReadLine()
            match line with
            |l when l = exitValue -> []
            |_ ->
                let n = Regex.Match(line,dataRegex)
                if n.Success then
                    [System.Double.Parse(n.Result("$1"));
                     System.Double.Parse(n.Result("$2"));
                     System.Double.Parse(n.Result("$3"));
                     System.Double.Parse(n.Result("$4"))]
                    ::read2Exit(exitValue)
                else
                    read2Exit(exitValue)

        let rec read () =
            if not sr.EndOfStream then
                match sr.ReadLine() with
                |"$$SOE" -> read2Exit("$$EOE")
                |_       -> read()
            else failwith "$$SOE not found"
        read()

    member val mass = setMass() with get
    member val radius = setRadius() with get
    member val data = dataCollect() with get

end


type SimulationFile (name:string) = class

    let path = name+"_Simulation.txt"

    let test = "(?<1>\d+)?\s(?<1>\d+)?\s(?<1>\d+)?"

    let writeFile (output:(int * (Vec3 list)))=
        use sw = new StreamWriter(path)
        let rec write(vecs:Vec3 list) =
            match vecs with
            |x::xs ->
                sw.WriteLine((x.X).ToString()+
                             " "+(x.Y).ToString()+
                             " "+(x.Z).ToString())
                write(xs)
            |[] -> ()
        sw.WriteLine(fst output)
        write(snd output)

    let readFile():(int * (Vec3 list)) =
        use sr = new StreamReader(path)
        let rec read():Vec3 list =
            if not sr.EndOfStream then
                let line = sr.ReadLine()
                let n = Regex.Match(line,test)
                let x =  new Vec3(System.Double.Parse(n.Result("$1")),
                                  System.Double.Parse(n.Result("$1")),
                                  System.Double.Parse(n.Result("$1")))
                x::read()
            else []
        (System.Int32.Parse(sr.ReadLine()),read())

    member this.ReadFile() =
        readFile()

    member this.WriteFile(output:(int * (Vec3 list))) =
        writeFile(output)

end


type FileInterface (name:string) = class

    member val SimulationData = new SimulationFile(name)

    member val ReadData = new ReadData(name)

end

let PlanetColour = function
| "Mercury" -> Color.Gray
| "Venus"   -> Color.DarkViolet
| "Earth"   -> Color.Blue
| "Mars"    -> Color.OrangeRed
| "Jupiter" -> Color.LemonChiffon
| "Saturn"  -> Color.Gold
| "Uranus"  -> Color.MediumOrchid
| "Neptune" -> Color.PaleTurquoise
| "Pluto"   -> Color.HotPink
| _ -> invalidArg "PlanetColour" "Could not find that planet name"

let LocalSystemFactory (s : string) =
  let rec calculateData(list:'a list list):Vec3 list =
          let calculate (list : 'a list) =
              let au = 149597870.
              let long:double  = list.[1]
              let lat:double   = list.[2]+90.
              let r = list.[3]*au
              let x = r * sin(lat  * System.Math.PI/180.) *
                          cos(long * System.Math.PI/180.)

              let y = r * sin(long * System.Math.PI/180.) *
                          sin(lat  * System.Math.PI/180.)
              new Vec3 (x,y,0.)

          match list with
          |[] -> []
          |x::xs -> calculate(x)::calculateData(xs)
  
  let reader = ReadData s
  let pPosLst = calculateData reader.data //Planet Position List
  let m = Mass(reader.radius, reader.mass, pPosLst.[1] - pPosLst.[0], pPosLst.[0])
  m.Name <- s
  LocalSystem(m)
let sunMass = 1.98855 * (10.0 ** 30.0) 
let sun = Mass(0.0, sunMass,Vec3(0.0,0.0,0.0),Vec3(0.0,0.0,0.0))
sun.Name <- "Sun"
let SolarSystem = LocalSystem(sun)
SolarSystem.AddLocalSystem <| LocalSystemFactory "Mercury"
SolarSystem.AddLocalSystem <| LocalSystemFactory "Venus"
SolarSystem.AddLocalSystem <| LocalSystemFactory "Earth"
SolarSystem.AddLocalSystem <| LocalSystemFactory "Mars"
SolarSystem.AddLocalSystem <| LocalSystemFactory "Jupiter"
SolarSystem.AddLocalSystem <| LocalSystemFactory "Saturn"
SolarSystem.AddLocalSystem <| LocalSystemFactory "Uranus"   // Added Ur-anus :-)
SolarSystem.AddLocalSystem <| LocalSystemFactory "Neptune"
SolarSystem.AddLocalSystem <| LocalSystemFactory "Pluto"

//Start Stopwatch
let stopWatch = System.Diagnostics.Stopwatch.StartNew()

// Simulate 364 days forwards, 24 times per day (we start at day 1)
SolarSystem.TS <- 3600uL  // 24 simulations per day - 1 per hour = 1 per 3600 s
SolarSystem.Simulate (364 * 24)

//Simulation stoped - let's see how long it took
stopWatch.Stop()
printfn "Simulation took: %f milliseconds" stopWatch.Elapsed.TotalMilliseconds

let solarSysPos = SolarSystem.GetPosList ()
let keep24thPos (x : Mass * (Vec3 list)) = 
  let newList =List.map snd (List.filter (fun x -> (fst x) % 24 = 0) (List.zip [0 .. List.length (snd x) - 1] (snd x)))
  ((fst x), newList)

let posToBeWritten = List.map keep24thPos solarSysPos

let iterFunc (x : Mass * (Vec3 list)) =
  let writer = SimulationFile((fst x).Name)
  writer.WriteFile ((fst x).ID, snd x)

List.iter iterFunc posToBeWritten

    
