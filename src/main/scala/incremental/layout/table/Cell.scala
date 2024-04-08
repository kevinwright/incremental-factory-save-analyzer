package incremental.layout
package table

import scala.quoted.*
import scala.compiletime.*

case class Cell(
    content: Content,
    overrideAlignment: Option[Alignment] = None
) {
  def mapContent(fn: Content => Content): Cell =
    new Cell(fn(content), overrideAlignment)
}

/** Special case because auto-tupling doesn't work on a single param  */
//inline def cells[T: ConvertsToCell](inline t: T): FixedList[1, Cell] =
//  FixedList.of(t.toCell)

transparent inline def cells[
  Tup <: NonEmptyTuple,
  W <: Tuple.Size[Tup] & Singleton
](
    inline tup: Tup
): FixedList[W, Cell] =
  cells_Untyped(tup).toFixedList.asInstanceOf[FixedList[W, Cell]]

private inline def cells_Untyped[Tup <: NonEmptyTuple](
    inline tup: Tup
): Tuple =
  ${ cells_Macro('tup) }

trait ConvertsToCell[A]:
  def convert(a: A): Cell
  extension (a: A) inline def toCell: Cell = convert(a)

///////////////
// Givens

given [A : ConvertsToContent]: ConvertsToCell[A] with
  inline def convert(x: A): Cell = Cell(x.toContent)

given ConvertsToCell[Cell] with
  inline def convert(x: Cell): Cell = x

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
            Expr.summon[ConvertsToCell[thead]] match {
              case Some(conv) => '{ ${ conv }.convert($headInExpr) }
              case _ =>
                report.errorAndAbort(
                  s"Cannot find an instance of ConvertsToCell[${Type.show[thead]}]"
                )
            }
        }
      val tailOutExpr = cells_Macro[ttail](tailInExpr)
      '{ $headOutExpr *: $tailOutExpr }
    case '[EmptyTuple] => '{ EmptyTuple }
}
