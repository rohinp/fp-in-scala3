package tdd.chapter14


/* 
data DoorResult = OK | Jammed

data DoorCmd : (ty : Type) -> DoorState -> (ty -> DoorState) -> Type where
  Open : DoorCmd DoorResult DoorClosed (\res => case res of 
                                                  OK => DoorOpen
                                                  Jammed => DoorClosed)
  Close : DoorCmd () DoorOpen (const DoorClosed)
  RingBell : DoorCmd () DoorClosed (const DoorClosed)
  Display : String -> DoorCmd () state (const state)
  
Pure : (res : ty) -> DoorCmd ty (state_fn res) state_fn
(>>=) : DoorCmd a state1 state2_fn ->
        ((res : a) -> DoorCmd b (state2_fn res) state3_fn) ->
        DoorCmd b state1 state3_fn
*/

object Test:

  /* 
  This is the closest implementation comparing with idris code i could get in scala3
  Though flatmap and map is not implemented but still it's just a bigenning to to implement
  state machines with validations.
  */
  enum DoorState:
    case DoorClosed
    case DoorOpen

  enum DoorResult:
    case OK
    case Jammed

  import DoorState._
  import DoorResult._

  type DoorClosed = DoorClosed.type
  type DoorOpen = DoorOpen.type
  type OK = OK.type
  type Jammed = Jammed.type

  type OpenType[DR] = DR match
    case OK => DoorOpen
    case Jammed => DoorClosed

  type Const[O] = [DR] =>> O

  trait DoorCmd[I,D,R[_]]:
    def apply(d:D):I => R[d.type]

  case object Open extends DoorCmd[DoorClosed, DoorResult, OpenType]:
    def apply(d:DoorResult):DoorClosed => OpenType[d.type] = 
      i => d match 
        case _:OK => DoorOpen
        case _:Jammed => i

  case object Close extends DoorCmd[DoorOpen, Unit, Const[DoorClosed]]:
    def apply(d:Unit):DoorOpen => Const[DoorClosed][d.type] = _ => DoorClosed

  case object RingBell extends DoorCmd[DoorClosed,Unit, Const[DoorClosed]]:
    def apply(d:Unit):DoorClosed => Const[DoorClosed][d.type] = _ => DoorClosed

@main def door =
  import Test._
  import DoorState._
  import DoorResult._

  val program = Close(()) compose Open(OK) compose RingBell(())

  println(program(DoorClosed))