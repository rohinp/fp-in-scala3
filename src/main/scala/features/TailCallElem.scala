package features

import scala.util.control.TailCalls._ 
  
/**
 * Example is dirrectly taken form the API doc.
 * Will come up with other exmaple of tail call WIP.
 */
def isEven(xs: List[Int]): TailRec[Boolean] = if (xs.isEmpty) done(true) else tailcall(isOdd(xs.tail)) 
def isOdd(xs: List[Int]): TailRec[Boolean] = if (xs.isEmpty) done(false) else tailcall(isEven(xs.tail)) 

def fib(n: Int): TailRec[Int] = 
    if (n < 2) done(n) else 
        for { 
            x <- tailcall(fib(n - 1)) 
            y <- tailcall(fib(n - 2)) 
        } yield x + y

@main def tailCallExample = {
    isEven((1 to 100000).toList).result
    fib(40).result
}