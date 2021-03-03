package provingground

object NaturalNumbers:
  enum Nat:
    case Zero
    case Succ(n:Nat)

  import Nat._

  val zero = Zero
  val _1 = Succ(zero)
  val _2 = Succ(_1)
  val _3 = Succ(_2)
  val _4 = Succ(_3)
  val _5 = Succ(_4)

  def doubleIt(nat:Nat):Nat = nat match
    case Zero => ???
    case Succ(n) => ??? 