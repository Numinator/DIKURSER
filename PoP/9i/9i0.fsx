
type Car (yearOfModel : int, make : string) = class
  let mutable speed = 0
  member this.YearOfModel = yearOfModel
  member this.Make = make
  member this.Accelerate = speed <- speed + 5
  member this.Brake = speed <- speed - 5; if speed < 0 then speed <- 0
  member this.GetSpeed = speed
end

let testCar = Car (1990, "Fiat")
for i in [1 .. 5] do
  testCar.Accelerate
  printfn "Acceleration %i; Current speed is %i" i (testCar.GetSpeed)
for i in [1 .. 5] do
  testCar.Brake 
  printfn "Braking %i; Current speed is %i" i (testCar.GetSpeed)


type CarWithGasExtension (yearOfModel : int, make : string) = class
  inherit Car(yearOfModel, make)
  let mutable gas = 0
  member this.AddGas amount = gas <- gas + amount
  member this.GasLeft = gas
  member this.Accelerate = 
    if gas > 0 then base.Accelerate; gas <- gas - 1;
    else ()
  member this.Brake =
    if gas > 0 then base.Brake ; gas <- gas - 1;
    else ()
end

let testCarGas = CarWithGasExtension(2002, "BMW")

//Gas with speed test 
printfn "\nTest with gas\n"
testCarGas.AddGas 1000
for i in [1 .. 1000] do
  testCarGas.Accelerate
printfn "Empty tank:\nGas amount at expected value: %b\n" (testCarGas.GasLeft = 0)

testCarGas.Accelerate
printfn "Accelerate with tank empty:"
printfn "Gas amount at expected value: %b" (testCarGas.GasLeft = 0)
printfn "Speed at expected value:      %b" (testCarGas.GetSpeed = 1000 * 5)

testCarGas.AddGas 1000
for i in [1 .. 1000] do
  testCarGas.Brake
testCarGas.Brake

printfn "\nBrake into negative speed:\nImpossible: %b" (testCarGas.GetSpeed = 0)
printfn "Gas amount at expected value: %b" (testCarGas.GasLeft = 0)

printfn "Drag race: Car with gas vs car that does not need"
printfn "Test car that need gas will run out halfway"
testCarGas.AddGas 500
for i in [1 .. 1000] do
  testCarGas.Accelerate
  testCar.Accelerate
printfn "Speed positions at expected values: %b" (testCarGas.GetSpeed * 2 = testCar.GetSpeed)