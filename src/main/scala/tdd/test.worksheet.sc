print("Hello")

case class Filter(run:Int => Either[String, Int]):  
  def chooseSuccess(other:Filter):Filter = Filter{
    a => run(a) match
        case Left(_) => Right(a)
        case ri@Right(r) => other.run(r) match 
          case Left(_) => Right(r)
          case x => x
  }

val filter1:Filter = Filter(x => Right(x*10))
val filter2:Filter = Filter(_ => Left("fuck up"))
val filter3:Filter = Filter(x => Right(x + 10))

filter1.chooseSuccess(filter2).run(2)
filter1.chooseSuccess(filter2).chooseSuccess(filter3).run(2)