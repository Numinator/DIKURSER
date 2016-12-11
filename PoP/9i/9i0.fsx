type Car (yearOfModel : int, make : string) = class
  let yearOfModel = yearOfModel
  let make = make
  let mutable speed = 0.0
  member this.YearOfModel = yearOfModel
  member this.Make = make
  member this.Accelerate () = speed <- speed + 5.0
  member this.Brake () = speed <- speed - 5.0; if speed < 0.0 then speed <- 0.0
  member this.GetSpeed () = speed
end

let testCar = Car (1990, "Fiat")
for i in [1 .. 5] do
  testCar.Accelerate ()
  printfn "Acceleration %i; Current speed is %f" i (testCar.GetSpeed ())
for i in [1 .. 5] do
  testCar.Brake ()
  printfn "Braking %i; Current speed is %f" i (testCar.GetSpeed ())

type CarWithGasExtension (yearOfModel : int, make : string) = class
  let internalCar = Car(yearOfModel, make)
  let gas = 0
  this.Accelerate () = if 

end
  