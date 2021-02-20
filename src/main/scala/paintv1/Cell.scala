package paintv1

import cats._
import cats.kernel._

case class Coordinate(x:Int, y:Int)

enum Cell:
    case BlankCell(coordinate:Coordinate)
    case OccupiedCell(coordinate:Coordinate)

    def coord:Coordinate = this match
        case BlankCell(co) => co
        case OccupiedCell(co) => co

object Cell:
    import Cell._

    given Semigroup[Cell] with
        def combine(x:Cell, y:Cell): Cell = 
            (x, y) match
                case (o:OccupiedCell, _:BlankCell) => o
                case (_:BlankCell, o:OccupiedCell) => o
                case _ => x

    extension (x:Cell) def combine(y:Cell): Cell = summon[Semigroup[Cell]].combine(x,y)
  
    given Show[Cell] with
        def show(c:Cell):String = 
            c match
                case Cell.BlankCell(_) => " "
                case Cell.OccupiedCell(_) => "."
  
    extension (c:Cell) def show:String = summon[Show[Cell]].show(c)
  
    extension (c:Cell) def occupy:Cell =
        c match
                case Cell.BlankCell(xs) => Cell.OccupiedCell(xs)
                case _ => c