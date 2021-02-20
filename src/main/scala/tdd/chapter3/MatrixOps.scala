package tdd.chapter3

import cats.kernel._
import cats.implicits._

import tdd.chapter4.Vect
import tdd.chapter4.Vect._
import tdd.chapter4.VectOps._


object MatrixOps:
    type Matrix[A, Row, Col] =  Vect[Vect[A, Col] , Row]

    def addVect[AA: Monoid ,CC <: Int](va:Vect[AA, CC], vb:Vect[AA, CC]): Vect[AA, CC] =
        map(zip(va,vb))(t => t._1 |+| t._2)

    def addMatrix[A:Monoid, Row <: Int, Col <: Int]: Matrix[A,Row,Col] => Matrix[A,Row,Col] => Matrix[A,Row,Col] = 
        ma => mb => 
            for {
                rowA <- ma
                rowB <- mb
            } yield addVect(rowA, rowB)
