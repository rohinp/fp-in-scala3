package features

import scala.quoted.*
import scala.compiletime.summonFrom

object TypeInfo:
  inline def apply[T[_]]: String = ${ typeInfoImpl[T] }

  def typeInfoImpl[T[_]](using qctx: Quotes, tpe: Type[T]): Expr[String] =
    import qctx.reflect.*
    
    ???
    
end TypeInfo
