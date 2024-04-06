package incremental.layout

import scala.quoted.*
import scala.compiletime.*
import compiletime.ops.int.*

object LayoutMacros {
  type HomogenousAux[H, T <: Tuple] = T match {
    case EmptyTuple => DummyImplicit
    case H *: t     => HomogenousAux[H, t]
    case _          => Nothing
  }

  type Homogenous[H] = [T <: Tuple] =>> HomogenousAux[H, T]

  type Repeat[N <: Int, T] <: Tuple = N match {
    case 0     => EmptyTuple
    case S[n1] => T *: Repeat[n1, T]
  }

  type SameSizeAs[TT <: Tuple, NT] <: Tuple = TT match {
    case EmptyTuple => EmptyTuple
    case _ *: t     => NT *: SameSizeAs[t, NT]
  }

  inline def exprTreeOf[T](t: T): String = ${ exprTreeOf_Macro('t) }

  def exprTreeOf_Macro[T: Type](
      t: Expr[T]
  )(using Quotes): Expr[String] = {
    import quotes.reflect.*
    val tpe: TypeRepr = TypeRepr.of[T]
    val typeString = tpe.show(using Printer.TypeReprStructure)
    Expr(typeString)
//    '{ $typeString }
  }

  //    AppliedType(
  //      TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Tuple3"),
  //      List(
  //        TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Int"),
  //        TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Int"),
  //        AppliedType(
  //          TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Option"),
  //          List(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Int"))
  //        )
  //      )
  //    )

  def cells_Macro[Tup <: Tuple: Type](
      tup: Expr[Tup]
  )(using Quotes): Expr[Tuple] = {
    import quotes.reflect.*
    Type.of[Tup] match
      case '[thead *: ttail] =>
        val headInExpr =
          '{ ${ tup.asExprOf[Tup with NonEmptyTuple] }.head }
            .asExprOf[thead]
        val tailInExpr =
          '{ ${ tup.asExprOf[Tup with NonEmptyTuple] }.tail }
            .asExprOf[ttail]

        val headOutExpr =
          Type.of[thead] match {
            case '[Cell] => headInExpr
            case _ =>
              Expr.summon[Conversion[thead, Cell]] match {
                case Some(conv) => '{ ${ conv }.convert($headInExpr) }
                case None =>
                  report.errorAndAbort(
                    s"Cannot find an instance of Conversion[${Type.show[thead]}, Cell]"
                  )
              }
          }
        val tailOutExpr = cells_Macro[ttail](tailInExpr)
        '{ $headOutExpr *: $tailOutExpr }
      case '[EmptyTuple] => '{ EmptyTuple }
  }

  inline def getTypeStringOfValue[A](value: A): String = ${
    getTypeStringImpl[A]
  }

  inline def getTypeString[A]: String = ${ getTypeStringImpl[A] }

  def getTypeStringImpl[A: Type](using Quotes): Expr[String] =
    Expr(Type.show[A])

  // transparent inline
  def stringify[
      Tup <: Tuple,
      S <: Tuple.Size[Tup]
  ](
      tup: Tup
  ): SameSizeAs[Tup, String] = {
    tup match {
      case EmptyTuple => EmptyTuple.asInstanceOf[SameSizeAs[Tup, String]]
      case x *: xs =>
        (x.toString *: stringify(xs)).asInstanceOf[SameSizeAs[Tup, String]]
      //      summonInline[Conversion[h.type, Cell]](h) *: cells(t)
    }
  }

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
}
