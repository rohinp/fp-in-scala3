package paintv1

import cats._
import cats.kernel._

type Shape = List[Cell]

object Shape:
    import Cell._

    given Monoid[Shape] with
        def combine(s1:Shape, s2:Shape):Shape =
            s2.foldLeft(s1){
                (acc, cell1) => {
                    acc.map(cell2 => if(cell2.coord == cell1.coord) cell2.combine(cell1) else cell2)
                }
            }
        def empty: Shape = List.empty[Cell]
    
    given Show[Shape] with
            def show(shape:Shape):String =
                
                def loop(prevRow: Int, acc: String, cells: List[Cell]): String =
                    cells match
                        case List() => acc
                        case OccupiedCell(Coordinate(row, _)) :: xs if prevRow != row => loop(row, acc + "\n" + ".", xs)
                        case OccupiedCell(Coordinate(row, _)) :: xs => loop(row, acc + ".", xs)
                        case BlankCell(Coordinate(row, _)) :: xs if prevRow != row => loop(row, acc + "\n" + " ", xs)
                        case BlankCell(Coordinate(row, _)) :: xs => loop(row, acc + " ", xs)
                    
                loop(0, "", shape)
  
    extension (s1:Shape) def combine(s2:Shape):Shape = summon[Monoid[Shape]].combine(s1,s2)
    
    extension (shape:Shape) def show:String = summon[Show[Shape]].show(shape)
    
    def canvas(height: Int, width: Int): Coordinate => Shape =
        co => 
            (for
                b <- co.x to width
                l <- co.y to height
            yield if(border(width,height)(Coordinate(b,l))) OccupiedCell(Coordinate(b, l)) else BlankCell(Coordinate(b, l))).toList

    def border(w:Int, h:Int)(coordinate: Coordinate):Boolean =
        List[Coordinate => Boolean](_.x == 0, _.y == 0, _.y == w, _.x == h).exists(_ (coordinate))

    def rectangle(length:Int,breadth:Int): Coordinate => Shape =
    originPoint =>
      (for
            b <- originPoint.x.toInt to (originPoint.x + length)
            l <- originPoint.y.toInt to (originPoint.y + breadth)
       yield OccupiedCell(Coordinate(b, l))).toList

    def square(side: Int): Coordinate => Shape = rectangle(side, side)

    def circle[T](radius: Int):Coordinate => Shape = originPoint =>
    (for
        degree <- 0 to 360
        x = radius * Math.cos(degree) + originPoint.x
        y = radius * Math.sin(degree) + originPoint.y
     yield OccupiedCell(Coordinate(x.toInt, y.toInt))).toList
   

enum Figure(f:Coordinate => Shape):
    case Canvas(height:Int, width:Int) extends Figure(Shape.canvas(height,width))
    case Circle(radius:Int) extends Figure(Shape.circle(radius))
    case Rectangle(length:Int,breadth:Int) extends Figure(Shape.rectangle(length,breadth))
    case Square(side:Int) extends Figure(Shape.square(side))

    def make(coordinate: Coordinate):Shape = 
      f(coordinate)