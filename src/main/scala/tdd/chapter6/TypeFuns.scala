package tdd.chapter6

import util.chaining.scalaUtilChainingOps
/**
 * StringOrInt : Bool -> Type
 * StringOrInt False = String
 * StringOrInt True = Int
 * 
 * getStringOrInt : (isInt : Bool) -> StringOrInt isInt
 * getStringOrInt False = "Ninety four"
 * getStringOrInt True = 94
 * 
 * DEPENDENT PATTERN MATCHING The getStringOrInt example illustrates a
 * technique that’s often useful when programming with dependent types:
 * dependent pattern matching.
*/
//emmulating the Type of idris in scala
//V1

//the idris version of StringOrInt in divided into 2 parts when it comes to scala3
//part 1 the trait which encodes the type and value
trait StringOrIntType {
    type B <: Boolean
    type StringOrInt = B match {
            case true => Int
            case false => String
        }
    val result:StringOrInt
}
//part 2 which is the actual dependent type
type StringOrIntDT = (b:StringOrIntType) => b.StringOrInt

//same for the getStringOrInt is divided into 2 parts
//part one is using the depedent type in a function
def getStringOrInt:StringOrIntDT = b => b.result
//part two is the implementations of the trait from part one which are the inputs for the function
object forFalse extends StringOrIntType:
    type B = false
    val result = "Ninety four"

object forTrue extends StringOrIntType:
    type B = true
    val result = 94

/**
 * Next interesting function to implement is 
 * 
 * valToString : (isInt : Bool) -> StringOrInt isInt -> String
 * valToString False y = trim y
 * valToString True y = cast y
 * 
 */
//I this case we might want to write a new dependent type function
type StringOrIntDT2 = (b:StringOrIntType) => String
//well the implementation in scala is quite easy as we have toString :-)
def valToString:StringOrIntDT2 = dt => dt.result.toString

@main def typeFuns = 
    getStringOrInt(forFalse).tap(println)
    getStringOrInt(forTrue).tap(println)
    valToString(forTrue).tap(println)
