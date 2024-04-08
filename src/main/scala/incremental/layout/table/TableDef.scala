package incremental.layout
package table

import cats.data.NonEmptyList

case class TableDef[
    W <: Int
](
    columnDefs: FixedList[W, ColumnDef]
) {
  def mapTitleContent(fn: Content => Content): TableDef[W] =
    new TableDef[W](
      columnDefs.map(colDef => colDef.withTitle(fn(colDef.title)))
    )

  def reify(
      rows: Seq[FixedList[W, Cell]]
  ): Table[W] = Table(this, rows)

  def build[T](
      source: IterableOnce[T],
      rowFn: T => FixedList[W, Cell]
  ): Table[W] = Table(
    this,
    source.iterator.map(rowFn).toSeq
  )
}

object TableDef {
  inline def apply[
      Tup <: NonEmptyTuple: Homogenous[ColumnDef],
      W <: Tuple.Size[Tup]
  ](
      inline columnDefs: Tup
  ): TableDef[W] = new TableDef[W](
    columnDefs.toFixedList.asInstanceOf[FixedList[W, ColumnDef]]
  )
}
