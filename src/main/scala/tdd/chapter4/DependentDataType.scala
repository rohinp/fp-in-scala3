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

enum Vehicle[T]:
    case Bicycle extends Vehicle[PowerSource.Pedal.type]
    case Car(fuel:Int) extends Vehicle[PowerSource.Petrol.type]
    case Bus(fuel:Int) extends Vehicle[PowerSource.Petrol.type]

/**
 * wheels : Vehicle power -> Nat
 * wheels Bicycle = 2
 * wheels (Car fuel) = 4
 * wheels (Bus fuel) = 4
 */
def wheels(v:Vehicle[PowerSource]): Int = v match 
    case Vehicle.Bicycle => 2
    case Vehicle.Car(_) => 4
    case Vehicle.Bus(_) => 4

/**
 * refuel : Vehicle Petrol -> Vehicle Petrol
 * refuel (Car fuel) = Car 100
 * refuel (Bus fuel) = Bus 200
 */
def refuel: Vehicle[PowerSource.Petrol.type] => Vehicle[PowerSource.Petrol.type] = {
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

@main def dependentDataType = 
    //refuel(Vehicle.Bicycle) compilation error
    refuel(Vehicle.Car(1))