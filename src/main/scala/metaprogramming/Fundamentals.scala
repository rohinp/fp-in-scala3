package metaprogramming

/**
 * In this fundamentals section we will see all the basics on Meta programming 
 * with simple examples and mode detailed explanation will follow in saperate sections
*/

/**
 * inline is a new soft modifier that guarantees that a definition will be inlined 
 * at the point of use. 
 * 
 * for example
*/
object InlineExample:
  inline def hello(msg:String):Unit = println(s"Hello workd with msg $msg")
  hello("replace with print")


//hello("awsome..") this will be onsite replace

/**
 * Other resources
 *  
 * 1. Details on different phases of compiler
 *    a. https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/core/Phases.scala
 *    b. http://dotty.epfl.ch/docs/internals/overall-structure.html#compiler-phases
 * 
 * 2. 
 * 3. 
 *
*/