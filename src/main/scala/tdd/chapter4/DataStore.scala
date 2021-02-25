package tdd.chapter4

import Vect._

/**
 * data DataStore : Type where
 *  MkData : (size : Nat) -> (items : Vect size String) -> DataStore
 * 
*/
sealed trait DataStore {
    type Size <: Int
    val size:Size
    val items:Vect[String, Size]
}
case class MkData[L <: Int](size:L, items:Vect[String, L]) extends DataStore{type Size = L}


/**
 * Using OO feature we could have directly accesed size and items no need for DFT defined below
*/
/**
 * size : DataStore -> Nat
 * size (MkData size' items') = size'
 */
type StoreSize = (ds:DataStore) => ds.Size

def size:StoreSize = ds => ds.size

/**
 * items : (store : DataStore) -> Vect (size store) String
 * items (MkData size' items') = items'
 */

type StoreItems = (ds:DataStore) => Vect[String, ds.Size]
def items:StoreItems = ds => ds.items

/**
 * addToStore : DataStore -> String -> DataStore
 * 
 * addToStore (MkData size items) newitem = MkData _ (addToData items)
 *  where
 *      addToData : Vect old String -> Vect (S old) String
 *      addToData [] = [newitem]
 *      addToData (item :: items) = item :: addToData items
 * 
*/
import VectOps._
import scala.compiletime._
//scala type system is not able to prove that data.size + 1 is a Size type which is inturn <: Int
//This is the reason we need an asInstanceOf to make it compile
//I not big of an issue but will check for a better solution for sure, that is this is still WIP
def addToStore(data:DataStore, newItem:String):DataStore = 
    MkData[S[data.Size]]((data.size + 1).asInstanceOf, newItem :: data.items)

import util.chaining.scalaUtilChainingOps

@main def dataStoreUsage = 
    size(MkData(2, VCons("1",VCons("2",VNil)))).tap(println)
    addToStore(MkData(2, VCons("1",VCons("2",VNil))), "test").tap(println)