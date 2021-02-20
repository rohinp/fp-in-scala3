package tdd.chapter1

object Prelude:
  /**
   * the is a type level identity function in idris
   * 
   * The first one is `the` function, 
   * basically an identity function
   * 
   * the : (ty : Type) -> ty -> ty
   * the ty x = x
   * 
   * example usage
   * Idris> :t the Int
   * the Int : Int -> Int
   * 
   * Idris> :t the String
   * the String : String -> String
   * 
   * Idris> :t the Bool
   * the Bool : Bool -> Bool
   * 
   * Idris> the Int 2
   * 2 : Int
   * 
   * We will attempt to convert the code to scala3 using dependent types
  * */
  trait The {
    type T 
    val id: T => T = identity
  }

  type TheDF = (x: The) => x.T => x.T

  def the:TheDF = t => t.id

  /**
   * output for above code
   * scala> :t the(new The{type T = Int})
   * Int => Int
   * scala> :t the(new The{type T = Int})(2)
   * Int
   * scala> the(new The{type T = Int})(2)
   * val res1: Int = 2
   * scala> :t the(new The{type T = String})
   * String => String
   * scala> :t the(new The{type T = Char})
   * Char => Char
   * */
  
  

    
