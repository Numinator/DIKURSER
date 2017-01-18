open System.IO
open System.Text.RegularExpressions


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

type ReadData (name:string) = class

    let filePath = name+".txt"

    let massRegex =
        "mass.*\d{2}\^(?<notation>\d{2}).*=\s*(?<mass>[0-9]\.[0-9]*)"
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