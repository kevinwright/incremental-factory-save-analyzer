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

  def format(customFormatFn: CustomContent => String): Table[W] =
    mapAllContent(c => TextContent(c.format(customFormatFn)))

  def mapAllContent(fn: Content => Content): Table[W] =
    Table[W](
      tableDef.mapTitleContent(fn),
      rows.map(_.map(_.mapContent(fn)))
    )

  def foldCellsByRow[T](
      z: FixedList[W, T]
  )(
      op: ((FixedList[W, T], FixedList[W, Cell]) => FixedList[W, T])
  ): FixedList[W, T] = {
    rows.foldLeft(z)((acc, row) => op(acc, row))
  }

  def foldByRow[T](
      z: FixedList[W, T]
  )(
      op: ((FixedList[W, T], FixedList[W, Content]) => FixedList[W, T])
  ): FixedList[W, T] = {
    rows.foldLeft(z)((acc, row) => op(acc, row.map(_.content)))
  }

  def mapCellsByRow[T](
      op: FixedList[W, Cell] => FixedList[W, Cell]
  ): Table[W] = Table[W](
    tableDef,
    rows.map(op)
  )

  def mapByColumn[U, T](
      seedOp: FixedList[W, ColumnDef] => FixedList[W, U],
      op: U => Content => Content
  ): Table[W] = {
    val seeds = seedOp(tableDef.columnDefs)
    Table[W](
      tableDef,
      rows map { row =>
        (seeds zip row) map ((seed, cell) => cell.mapContent(op(seed)))
      }
    )
  }

  def mapReduceColumns[T](
      initFn: ColumnDef => T,
      mapFn: Content => T,
      reduceFn: (T, T) => T
  ): FixedList[W, T] =
    foldByRow(tableDef.columnDefs.map(initFn)) { (acc, row) =>
      acc.zip(row.map(mapFn)).map((acc, row) => reduceFn(acc, row))
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
