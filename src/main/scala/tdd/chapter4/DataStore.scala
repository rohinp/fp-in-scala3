package tdd.chapter4

import Vect._
import VectOps._

import scala.compiletime._
import scala.compiletime.testing._
import util.chaining.scalaUtilChainingOps

import cats.implicits._

/**
 * data DataStore : Type where
 *  MkData : (size : Nat) -> (items : Vect size String) -> DataStore
 * 
*/
sealed trait DataStore {
    type Size <: Int
    val items:Vect[String, Size]
    val size:Size = length(items)
}
case class MkData[L <: Int](items:Vect[String, L]) extends DataStore{type Size = L}


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

def addToStore(data:DataStore, newItem:String):DataStore = 
    MkData(newItem :: data.items)


/**
 * data Command = Add String
 *      | Get Integer
 *      | Quit 
 * */
enum Command:
    case Add(add:String)
    case Get(get:Int)
    case Quit

/**
 * parseCommand : String -> String -> Maybe Command
 * parseCommand "add" str = Just (Add str)
 * parseCommand "get" val = case all isDigit (unpack val) of
 *                              False => Nothing
 *                              True => Just (Get (cast val))
 * parseCommand "quit" "" = Just Quit
 * parseCommand _ _ = Nothing
*/
def parseCommand(cmd:String, input:String): Option[Command] = 
    (cmd, input) match 
        case ("add", str) => Command.Add(str).some
        case ("get", value) => value.toIntOption.map(Command.Get(_))
        case ("quit", "") => Command.Quit.some
        case ( _ , _ ) => none[Command]

/**
 * parse : (input : String) -> Maybe Command
 * parse input = case span (/= ' ') input of (cmd, args) => parseCommand cmd args
 * */
def parse(input:String): Option[Command] = input.span(_ != ' ').pipe{(cmd, args) => parseCommand(cmd, args)}

/**
 * getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
 * getEntry pos store = 
 *      let store_items = items store in case integerToFin pos (size store) of
 *                                                          Nothing => Just ("Out of range\n", store)
 *                                                          Just id => Just (index id (items store) ++ "\n", store)
*/
def getEntry(pos:Int, store: DataStore) : Option[(String, DataStore)] = ???



/**
 * processInput : DataStore -> String -> Maybe (String, DataStore)
 * processInput store inp = case parse inp of
 *                              Nothing => Just ("Invalid command\n", store)
 *                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
 *                              Just (Get pos) => getEntry pos store
 *                              Just Quit => Nothing
 */
 def processInput(store:DataStore, input:String): Option[(String, DataStore)] =
    parse(input).fold(("Invalid command\n", store).some){
        case Command.Add(item) => (s"ID ${size(store)} \n", addToStore(store, item)).some
        case Command.Get(pos) => ???
        case Command.Quit => none
    }



@main def dataStoreUsage = 
    size(MkData(VCons("1",VCons("2",VNil)))).tap(println)
    addToStore(MkData(VCons("1",VCons("2",VNil))), "test").tap(println)
    //typeCheckErrors(addToStore).tap(println)