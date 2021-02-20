package paintv2

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import util.chaining.scalaUtilChainingOps
import paintv1.Shape
import paintv1.Figure._
import paintv1.Coordinate
import Shape.given
import Shape._

case class Paint[A](draw: Shape => (A, Shape)):

  def result[B]: B => Paint[B] = b => Paint(s => (b, s))

  def map[B](f: A => B): Paint[B] = flatMap(shape => result(f(shape)))
  
  def flatMap[B](f: A => Paint[B]): Paint[B] =
    Paint(shape => {
      val (a, shape1) = draw(shape)
      f(a).draw(shape1)
    })

type PaintWithContext = ExecutionContext ?=> Future[Paint[Shape]]

def paintIt(shape: Shape): PaintWithContext =
  Future(Paint(canvas => (shape, canvas.combine(shape))))

def get: PaintWithContext = Future(Paint(p => (p, p)))

@main
def run() =

  val program: PaintWithContext = 
    for 
        _ <- paintIt((Square(3).make(Coordinate(5, 5))))
        _ <- paintIt((Rectangle(20, 10).make(Coordinate(10, 10))))
        _ <- paintIt((Circle(15).make(Coordinate(40, 40))))
        s <- get
    yield s
    
  given ec:ExecutionContext =  concurrent.ExecutionContext.Implicits.global

  program
    .map(
      _.draw(Canvas(100, 100).make(Coordinate(0, 0)))._2
    .show
    .pipe(println)
    )


    