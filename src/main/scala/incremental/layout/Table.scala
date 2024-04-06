package incremental.layout

import cats.data.NonEmptyList
import incremental.layout.Alignment.{left, right}

import LayoutMacros.*

import Tuple.{Size, Union}
import compiletime.ops.int.*
import scala.compiletime.{erasedValue, error, summonInline, summonFrom}
import scala.language.implicitConversions

case class ColumnDef(
    title: Content,
    titleAlignment: Alignment = Alignment.auto,
    alignment: Alignment = Alignment.auto
)

case class Cell(
    value: Content,
    overrideAlignment: Option[Alignment] = None
)

/** Special case because auto-tupling doesn't work on a single param
  */
//inline def cells[T](inline t: T)(using
//    conv: Conversion[T, Content]
//): Tuple1[Cell] =
//  Tuple1(Cell(conv(t)))

inline def cells[Tup <: NonEmptyTuple](
    inline tup: Tup
): SameSizeAs[Tup, Cell] =
  cells_Untyped(tup).asInstanceOf[SameSizeAs[Tup, Cell]]

inline def cells_Untyped[Tup <: NonEmptyTuple](
    inline tup: Tup
): Tuple =
  ${ cells_Macro('tup) }

given [T](using conv: Conversion[T, Content]): Conversion[T, Cell] with
  def apply(t: T): Cell = Cell(conv(t))

case class TableDef[
    ColsTupleT <: NonEmptyTuple
](
    columnDefs: ColsTupleT
)(using HomogenousAux[ColumnDef, ColsTupleT]) {

  // If this alias is actually used in `reify` and `build` then we get errors like
  //   Found: (incremental.layout.Cell, incremental.layout.Cell)
  //   Required: incremental.layout.TableDef.tableDef.RowT
  //
  // But if it's repeated you get
  //   Found:    (incremental.layout.Cell, incremental.layout.Cell)
  //   Required: (incremental.layout.Cell, incremental.layout.Cell, incremental.layout.Cell)
  //
  // So it's best to only use this alias externally
  type RowsTupleT = SameSizeAs[ColsTupleT, Cell]

  val columnDefList =
    NonEmptyList(columnDefs.head, columnDefs.tail.toList)
      .asInstanceOf[NonEmptyList[ColumnDef]]

  def reify(
      rows: Seq[SameSizeAs[ColsTupleT, Cell]]
  ): Table[ColsTupleT, SameSizeAs[ColsTupleT, Cell]] =
    Table(this, rows)

  def build[
      T,
      ValueRowT <: NonEmptyTuple
  ](
      source: IterableOnce[T],
      rowFn: T => SameSizeAs[ColsTupleT, Cell]
  ): Table[ColsTupleT, SameSizeAs[ColsTupleT, Cell]] = Table(
    this,
    source.iterator.map(rowFn).toSeq
  )
}

case class Table[
    ColsTupleT <: NonEmptyTuple: Homogenous[ColumnDef],
    RowsTupleT <: SameSizeAs[ColsTupleT, Cell]
](
    tableDef: TableDef[ColsTupleT],
    rows: Seq[RowsTupleT]
)

object TableDef {

  val tableDef = TableDef(
    columnDefs = (
      ColumnDef(title = "a", alignment = left),
      ColumnDef(title = "b", alignment = right),
      ColumnDef(title = "c", alignment = right)
    )
  )

  val oneRow = cells(
    Cell("1"),
    Cell("2"),
    Cell("3")
  )

  val testReify = tableDef.reify(Seq(oneRow))


  val testBuild = tableDef.build(
    Seq(1, 2, 3),
    value =>
      (
        Cell(s"a$value"),
        Cell(s"b$value"),
        Cell(s"c$value")
      )
  )

  def main(args: Array[String]): Unit = {
//    println(exprTreeOf(tableDef))
    pprint.pprintln(testReify)
    pprint.pprintln(testBuild)
  }
}
