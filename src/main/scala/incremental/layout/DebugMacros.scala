package incremental.layout

import scala.quoted.*
import scala.compiletime.*
import compiletime.ops.int.*

inline def exprTreeOf[T](t: T): String = ${ exprTreeOf_Macro('t) }

def exprTreeOf_Macro[T: Type](
    t: Expr[T]
)(using Quotes): Expr[String] = {
  import quotes.reflect.*
  val tpe: TypeRepr = TypeRepr.of[T]
  val typeString = tpe.show(using Printer.TypeReprStructure)
  Expr(typeString)
}

inline def getTypeStringOfValue[A](value: A): String = ${
  getTypeStringImpl[A]
}

inline def getTypeString[A]: String = ${ getTypeStringImpl[A] }

def getTypeStringImpl[A: Type](using Quotes): Expr[String] =
  Expr(Type.show[A])


inline def printTypeInline[
    Tup <: Tuple,
    S <: Tuple.Size[Tup]
](
    inline tup: Tup
): Unit = {
  println("inline type: " + getTypeString[Tup])
  println("inline size: " + getTypeString[S])
  summonFrom {
    case _: (Tup =:= EmptyTuple) =>
      println("empty")
    case conv: Conversion[Tuple.Head[Tup with NonEmptyTuple], Content] =>
      println(
        "converted head: " + conv(
          tup.asInstanceOf[Tup with NonEmptyTuple].head
        )
      )
    case _ =>
      println("head: " + tup.asInstanceOf[Tup with NonEmptyTuple].head)
      println("non-empty")
  }
}
