package incremental.layout

import incremental.layout.table.*

case class Table[
    W <: Int
](
    tableDef: TableDef[W],
    rows: Seq[FixedList[W, Cell]]
) {
  def rowContents(
      fn: Content => Content
  ): Seq[FixedList[W, Content]] =
    rows.map(row => row.map(_.content))

  def mapAllContent(fn: Content => Content): Table[W] =
    Table[W](
      tableDef.mapTitleContent(fn),
      rows.map(_.map(_.mapContent(fn)))
    )

  inline def foldRows[T](
      z: FixedList[W, T]
  )(
      op: ((FixedList[W, T], FixedList[W, Cell]) => FixedList[W, T])
  ): FixedList[W, T] = {
    rows.foldLeft(z)((acc, row) => op(acc, row))
  }

  def foldRowContent[T](
      z: FixedList[W, T]
  )(
      op: ((FixedList[W, T], FixedList[W, Content]) => FixedList[W, T])
  ): FixedList[W, T] = {
    rows.foldLeft(z)((acc, row) => op(acc, row.map(_.content)))
  }
}

object Table {
  inline def tableDef[
      Tup <: NonEmptyTuple : Homogenous[ColumnDef],
      W <: Tuple.Size[Tup]
  ](
      inline columnDefs: Tup
  ): TableDef[W] = new TableDef[W](
    columnDefs.toFixedList.asInstanceOf[FixedList[W, ColumnDef]]

  )
}
