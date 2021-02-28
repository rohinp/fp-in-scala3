package tdd.chapter4

import scala.util.chaining._
import cats.kernel._
import cats.implicits._
import scala.compiletime.S
import scala.compiletime.ops.int._

enum Vect[+E, L]:
  case VNil extends Vect[Nothing, 0]
  case VCons[+E, L <: Int](head: E, tail: Vect[E,L]) extends Vect[E, S[L]]

object VectOps:
  import Vect._

  def pure[A](a:A):Vect[A,1] = VCons(a, VNil)
  
  def head[E](v:Vect[E,_]):Option[E] = v match
    case VNil => None
    case VCons(h,_) => Some(h)

  extension [E, M <: Int](x: E) def ::(xs: Vect[E,M]):Vect[E, S[M]] = xs match {
    case VNil => pure(x)
    case _ => VCons(x, xs)
  }

  extension [E, M <: Int, N <: Int](xs: Vect[E,M]) def ++(ys: Vect[E,N]):Vect[E, M + N] = xs match {
    case VNil => ys.asInstanceOf[Vect[E, M + N]]
    case VCons(x, xs) => VCons(x, xs ++ ys).asInstanceOf[Vect[E, M + N]]
  }
  
  extension [A, B, M <: Int, N <: Int](va:Vect[A,M]) def flatMap(f: A => Vect[B, N]): Vect[B, M] =
    va match
      case VNil => VNil
      case VCons(head, tail) => (f(head) ++ (tail flatMap f)).asInstanceOf[Vect[B, M]]

  extension [A, B, L <: Int](va:Vect[A, L]) def map(f: A => B): Vect[B, L] = va flatMap (a => pure(f(a)))
  
  def zip[A, B, L](v1: Vect[A,L], v2: Vect[B,L]):Vect[(A,B),L] = (v1,v2) match {
    case (VCons(x, xs), VCons(y, ys)) => VCons((x,y), zip(xs, ys))
    case (VNil, VNil) => VNil
  }

  //scala type system is not able to prove that 1 + length(tail) is a L type which is in turn <: Int
  //This is the reason we need an asInstanceOf to make it compile
  //It's not big of an issue but will check for a better solution for sure, so it's WIP
  def length[L <: Int](v: Vect[_,L]):L = v match
    case VNil => 0
    case VCons(_, tail) => (1 + length(tail)).asInstanceOf[L]
    
  trait VectIndex {
    type Data
    type Index <: Int
    type Length
    val index:Index
    val vect:Vect[Data, Length]
    type ValidIndexProof = (0 <= Index <= Length) match {
      case true => true =:= true
      case _ => Unit
    }
  }
  
  def index(vi:VectIndex)(using vi.ValidIndexProof):vi.Data = 
    def loop[L](i:Int, items:Vect[vi.Data,L]): vi.Data = items match 
      case VCons(data, tail) if i == 0 => data
      case VCons(_, tail) => loop(i - 1, tail)
      case _ => loop(i, items) //this will never happen
    loop(vi.index, vi.vect)

@main def vectUsage =
  import VectOps._
  import Vect._
  println("Creating a Vect with single element")
  val singletonVect = VCons("a", VNil)
  println(singletonVect)
  println("Adding more elements to Vect")
  val vect1 = VCons("b", singletonVect)
  val vect11 = VCons("c", singletonVect)
  println(vect1)
  println("As we can see above the Vect type encodes both data and length of vect information")
  println("Now lets take an example of zip")
  val vect2 = VCons("d", vect1)
  println("val result = zip(vect1, vect2) wont work as we can only zip vect of same size unlike normal " +
    "list where you can zip list of diff size and many a times it doesnt make sense, " +
    "and all this works on compile time because your type encodes the information of size in itself")
  //val result = zip(vect1, vect2)
  val result = zip(vect1, vect11)
  println(s"val result = zip(vect1, vect11) this will work as both vect are of same size and result is $result")

  println("Combinators map and flatmap example with for expression")
  (for {
    x <- vect2
    y <- vect1
  } yield (x , y)).tap(println)
  //using the consoperator of Vect
  (1 :: (2 :: VNil)).tap(println)
  //compile check index function
  index(new VectIndex{
    type Data = String
    type Index = 1
    type Length = 3
    val  index = compiletime.constValue[Index]
    val vect = VCons("1", VCons("2", VCons("3", VNil)))
  }).tap(println)