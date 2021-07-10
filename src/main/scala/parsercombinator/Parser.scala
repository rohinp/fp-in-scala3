package parsercombinator

import util.chaining.scalaUtilChainingOps

/**
 * <p> Rather than trying to anticipate all use cases upfront, an alternative approach is to choose a representation 
 * that preserves as much structure as possible, so that alternative interpreters may be added later on. 
 * To do that, we’re effectively going to enumerate the set of parsers, and ways of combining them, 
 * by defining a type for representing an abstract syntax tree (AST). To this purpose, we’ll use a GADT, 
 * that also expresses that different parsers are indexed by different types. 
 * The initial constructors are split into two sets – 
 * primitive ones (the leaf nodes of the tree), 
 * and combinators for composing parsers:
*/
enum Parser[+T]:
  case Fail[T](msg:String) extends Parser[T]
  case Empty extends Parser[Nothing]
  case Pure[T](t:T) extends Parser[T]
  case Symbol(ch:Char) extends Parser[Char]
  case Map[A,B](f:A => B, pa:Parser[A]) extends Parser[B]
  case Product[A,B](a:Parser[A], b:Parser[B]) extends Parser[(A,B)]
  case Either[A](a1:Parser[A], a2:Parser[A]) extends Parser[A]
  case Delay[T](pa: Unit => Parser[T]) extends Parser[T]
  case Fix[T](f:Parser[T] => Parser[T]) extends Parser[T]

/**
 * The primitive parsers are:
 * 
 * 1. Fail msg - a parser that always fails.
 * 2. Empty - a parser that succeeds when given empty input.
 * 3. Return x - a parser that does not consume any input and always returns x.
 * 4. Symbol c - a parser that matches input when the first character is c.
 * 5. The applicative interface is what provides sequential composition, as in: first parse x with parser p1, 
 * then parse y with parser p2 and combine their results.
 * 
 * The Either constructor makes it possible to provide alternative execution paths, i.e. 
 * parsers that succeed on different types of input.
*/

object ParserOps:
  import Parser._
  //smart constructors
  def empty[T]:Parser[T] = Empty
  def fail[T](msg:String) = Fail[T](msg)
  def pure[T](t:T):Parser[T] = Pure(t)
  def symbol(ch:Char):Parser[Char] = Symbol(ch)
  def map[A,B](fa:Parser[A])(f:A => B):Parser[B] = Map(f,fa)
  def product[A,B](fa:Parser[A], fb:Parser[B]):Parser[(A,B)] = Product(fa,fb)
  def either[A](fa:Parser[A], fb:Parser[A]):Parser[A] = Either(fa,fb)
  def delay[T](f: => Parser[T]):Parser[T] = Delay(_ => f)
  def fix[T](f: Parser[T] => Parser[T]):Parser[T] = Fix(f)

  //Syntax functions
  extension [A,B] (fa:Parser[A]) def <&>(f:A => B):Parser[B] = map(fa)(f)
  extension [A] (pa1:Parser[A]) def <|>(pa2:Parser[A]):Parser[A] = either(pa1,pa2)
  extension [A,B] (pf:Parser[A => B]) def <*>(pa:Parser[A]):Parser[B] = 
        product(pf,pa) <&> ((f,a) => f(a))
  extension [A,B] (pa:Parser[A]) def *> (pb:Parser[B]):Parser[B] =
    product(pa,pb) <&> ((a,b) => b)
  extension [A,B] (pa:Parser[A]) def <* (pb:Parser[B]):Parser[A] = 
    product(pa,pb) <&> ((a,b) => a)
  extension [A,B] (a:A) def |> (f:A => B):B = a.pipe(f)
  
  //utility functions
  //identity function we already have in scala
  val const = [A,B] => (a:A,b:B) => a
  extension [A,B,C] (f:A => B) def >> (g:B => C):A => C = g compose f
  //String to char list and list of char to string is already there in scala

  //Building simple parsers

  def string:String => Parser[String] = 
    _.foldLeft(pure(""))((p, c) => {
      p <&> (str => s"$str$c")
    })

/*   def digit = 
    "0123456789"
      |> (_.map(symbol))
      |> (_.foldLeft(fail[Char]("Fail to parse, non digit Char"))((acc,v) => either(acc,v))) */

  def choose[T]:Seq[Parser[T]] => Parser[T] = 
    _.foldLeft(fail[T]("None of the choises worked"))((acc,v) => either(acc,v))

  def oneOf:String => Parser[Char] = in => in.map(symbol) |> choose

  //better version of digit
  def digit = oneOf("0123456789")

/*   def many[T](p:Parser[T]):List[Parser[T]] =
    def manyOne =
      val x = p
      val xs = many
      x::xs
      either(manyOne, pure(List())) */