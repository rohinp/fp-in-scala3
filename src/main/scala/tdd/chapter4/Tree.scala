package tdd.chapter4

/**
 * Recursive data type
 * 
 * data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
 * 
 * insert : Ord elem => elem -> Tree elem -> Tree elem
 * insert x Empty = Node Empty x Empty
 * insert x (Node left val right) = case compare x val of
 *                                      LT => Node (insert x left) val right
 *                                      EQ => Node left val right
 *                                      GT => Node left val (insert x right)
 * 
  */
import cats._
import cats.implicits._

enum Tree[+E]:
    case Empty
    case Node(left:Tree[E], data:E, right:Tree[E])

import Tree._

def insert[E:Order](elem:E, tree:Tree[E]): Tree[E] = 
    (elem, tree) match
        case (e, Empty) => Node(Empty, e, Empty)
        case (e, Node(l, d, r)) if e < d => Node(insert(e,l), d, r)
        case (e, Node(l, d, r)) if e > d => Node(l, d, insert(e,r))
        case (_, n@Node(_, _, _))  => n


/**
 * Encoding the contraint at the type level.
 * 
 * data BSTree : Type -> Type where
 *      Empty : Ord elem => BSTree elem
 *      Node : Ord elem => (left : BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem
 * 
 * insert : elem -> BSTree elem -> BSTree elem
 * insert x Empty = Node Empty x Empty
 * insert x orig@(Node left val right) = case compare x val of
 *                                          LT => Node (insert x left) val right
 *                                          EQ => orig
 *                                          GT => Node left val (insert x right)
  */

type BSTree[T] = Order[T] ?=> Tree[T]

def insert_bs[E](elem:E, tree:BSTree[E]): BSTree[E] = 
    (elem, tree) match
        case (e, Empty) => Node(Empty, e, Empty)
        case (e, Node(l, d, r)) if e < d => Node(insert_bs(e,l), d, r)
        case (e, Node(l, d, r)) if e > d => Node(l, d, insert_bs(e,r))
        case (_, n@Node(_, _, _))  => n
        