package tdd.chapter4

/**
 * Defining dependent data types
 * A first example: classifying vehicles by power source
 * 
 * data PowerSource = Petrol | Pedal
 * 
 * data Vehicle : PowerSource -> Type where
 *  Bicycle : Vehicle Pedal
 *  Car : (fuel : Nat) -> Vehicle Petrol
 *  Bus : (fuel : Nat) -> Vehicle Petrol
 */

enum PowerSource :
    case Petrol
    case Pedal

type Pedal = PowerSource.Pedal.type
type Petrol = PowerSource.Petrol.type

//Dependent type data type
enum Vehicle[T <: PowerSource]:
    case Bicycle extends Vehicle[Pedal]
    case Car(fuel:Int) extends Vehicle[Petrol]
    case Bus(fuel:Int) extends Vehicle[Petrol]

trait VehicleType {
    type P <: PowerSource
    val resultValue:Vehicle[P]
}

/**
 * wheels : Vehicle power -> Nat
 * wheels Bicycle = 2
 * wheels (Car fuel) = 4
 * wheels (Bus fuel) = 4
 */
//v1 use dependent function type
type Wheel_DTF = (vt:VehicleType) => Int

def wheels_v1:Wheel_DTF = vt => vt.resultValue match {
    case Vehicle.Bicycle => 2
    case Vehicle.Car(_) => 4
    case Vehicle.Bus(_) => 4
}

//v2 directly using dependent data type
def wheels_v2(v:Vehicle[PowerSource]): Int = ??? /* v match 
    case Vehicle.Bicycle => 2
    case Vehicle.Car(_) => 4
    case Vehicle.Bus(_) => 4 */

/**
 * refuel : Vehicle Petrol -> Vehicle Petrol
 * refuel (Car fuel) = Car 100
 * refuel (Bus fuel) = Bus 200
 */
//v1 use dependent function type
type Refuel_DTF = (vt:VehicleType{type P = Petrol}) => Vehicle[vt.P]

def refuel_v1:Refuel_DTF = vt => vt.resultValue match {
    case  Vehicle.Car(_) => Vehicle.Car(100)
    case Vehicle.Bus(_) => Vehicle.Bus(200)
}

//v2 directly using dependent data type
def refuel_v2: Vehicle[Petrol] => Vehicle[Petrol] = {
    case  Vehicle.Car(_) => Vehicle.Car(100)
    case Vehicle.Bus(_) => Vehicle.Bus(200)
}

/**
 * DEFINING FAMILIES OF TYPES For Vehicle, you’ve actually defined two types
 * in one declaration (specifically, Vehicle Pedal and Vehicle Petrol).
 * Dependent data types like Vehicle are therefore sometimes referred to as
 * families of types, because you’re defining multiple related types at the same
 * time. The power source is an index of the Vehicle family. The index tells
 * you exactly which Vehicle type you mean
 */
import util.chaining.scalaUtilChainingOps

@main def dependentDataType = 
    //compilation errore
    //refuel_v1(new VehicleType{type P = Pedal; val resultValue = Vehicle.Bicycle})
    //refuel_v2(Vehicle.Bicycle) 
    //correct calls
    refuel_v1(new VehicleType{type P = Petrol; val resultValue = Vehicle.Car(2)}).pipe(println)
    refuel_v2(Vehicle.Car(2)).pipe(println)