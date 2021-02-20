package tdd.chapter4

/**
 * In this we will compare to the user defined types in idris
 * with those in scala3
 * 
 * The first one is Enumerations type
 * example: 
 * {{
 *  data Direction = North | East | South | West
 * }}
 * 
 * And similar construct is avaialble in scala3
 */
 enum Direction:
    case North
    case East
    case South
    case West

/**
 * Example usage
 * {{
 * turnClockwise : Direction -> Direction
 * turnClockwise North = East
 * turnClockwise East = South
 * turnClockwise South = West
 * turnClockwise West = North
 * }}
 * 
 * */
import Direction._

def turnClockwise: Direction => Direction = 
    case North => East
    case East => South
    case South => West
    case West => North

/**
 * Obviously there is more that can be done with Enum types
 * We will se more examples later or this chapter or you can refer to 
 * different versions of paint app, where enums are used.
 * 
*/

/**
 * Next is union types.
 * 
 * {{
 * data Shape = Triangle Double Double
 *                  | Rectangle Double Double
 *                  | Circle Double
 * 
 * area : Shape -> Double
 * area (Triangle base height) = 0.5 * base * height
 * area (Rectangle length height) = length * height
 * area (Circle radius) = pi * radius * radius
 * }}
 *
 * In scala3 we can do this using same enum type
*/
enum Shape:
    case Triangle(base:Double, height:Double)
    case Rectangle(length:Double, height:Double)
    case Circle(radius:Double)

def area: Shape => Double = 
    case Shape.Triangle(b, h) => 0.5 * b * h
    case Shape.Rectangle(l, h) => l * h
    case Shape.Circle(r) => Math.PI * r * r

/**
 * data Picture = Primitive Shape
 *                  | Combine Picture Picture
 *                  | Rotate Double Picture
 *                  | Translate Double Double Picture
*/

enum Picture:
    case Primitive(shape:Shape)
    case Combine(p1:Picture, p2:Picture)
    case Rotate(d:Double, picture:Picture)
    case Translate(x:Double, y:Double, picture:Picture)

