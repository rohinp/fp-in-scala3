package provingground

object BooleanType :
  //In scala 3 we can define a boolean type
  //And also tread the values at type level
  import scala.compiletime.ops.boolean._
  //the above import is new in scala 3 which provides some type level operations on boolean
  //Type level not operation
  type A = true
  type B = ![A]
  //Type level and operation
  type C = &&[A,B]
  