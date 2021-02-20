package paintv1

import util.chaining.scalaUtilChainingOps

case class Paint[A](draw: Shape => (A, Shape)):

  def result[B]: B => Paint[B] = b => Paint(s => (b, s))

  def map[B](f: A => B): Paint[B] = flatMap(shape => result(f(shape)))
  
  def flatMap[B](f: A => Paint[B]): Paint[B] =
    Paint(shape => {
      val (a, shape1) = draw(shape)
      f(a).draw(shape1)
    })

@main
def run() =
  import Figure._
  import Shape.given
  import Shape._
    
  def paintIt(shape: Shape): Paint[Shape] =
    Paint(canvas => (shape, canvas.combine(shape)))

  def get: Paint[Shape] = Paint(p => (p, p))

  val program: Paint[Shape] = 
    for 
        _ <- paintIt((Square(3).make(Coordinate(5, 5))))
        _ <- paintIt((Rectangle(20, 10).make(Coordinate(10, 10))))
        _ <- paintIt((Circle(15).make(Coordinate(40, 40))))
        s <- get
    yield s

  program
    .draw(Canvas(100, 100).make(Coordinate(0, 0)))._2
    .show
    .pipe(println)


    