package incremental.layout
package table

import scala.util.Success

object TableTest {
  val tableDef: TableDef[4] = TableDef(
    ColumnDef(title = "a", alignment = Alignment.left),
    ColumnDef(title = "b", alignment = Alignment.right),
    ColumnDef(title = "c", alignment = Alignment.right),
    ColumnDef(title = "custom", alignment = Alignment.left)
  )

  val oneRow: FixedList[4, Cell] =
    cells("1", 2, 3.0d, WrappedCustomContent(scala.util.Success("hi mum")))

  val testReify: Table[4] = tableDef.reify(Seq(oneRow))

  val testBuild: Table[4] = tableDef.build(
    Seq(3, 6, 9),
    value =>
      cells(
        "a" * value,
        "b" * value,
        "c" * value,
        WrappedCustomContent(s"c$value")
      )
  )

  def customFormatFunc(content: Content): String = content match {
    case WrappedCustomContent(s: String)  => s"Custom String: [$s]"
    case WrappedCustomContent(Success(x)) => s"Custom Success [${x.toString}]"
    case other                            => s"Other: [$other]"
  }

  val testMapped: Table[4] = testReify.mapAllContent(
    _.format(customFormatFunc).toContent
  )

  val fl = FixedList.of("w", "x", "y", "z")
  println(getTypeStringOfValue(fl))
  pprint.pprintln(fl)

  val ufl = FixedList.unsafeFromList[4](List(1, 2, 3, 4))
  println(getTypeStringOfValue(ufl))
  pprint.pprintln(ufl.map(_ * 2))

  val mapped = ufl.flatMap(x => FixedList.of(x.toString, (10 * x).toString))
  println(getTypeStringOfValue(mapped))
  pprint.pprintln(mapped)

  val formatted =
    testBuild.format(customFormatFunc)

  val widths =
    formatted.mapReduceColumns(
      initFn = _.title.format().length,
      mapFn = _.format().length,
      reduceFn = _ max _
    )

  val padded =
    formatted.mapByColumn(
      seedOp = _ => widths,
      op = width =>
        content =>
          TextContent(
            content.format().padTo(width, '∙')
          )
    )

  val testFold: FixedList[4, String] =
    testBuild.foldByRow(fl) { (acc, row) =>
      val strings = row.map(_.format(customFormatFunc))
      acc.zip(strings).map((a, b) => s"$a+$b")
    }

  def main(args: Array[String]): Unit = {
    println("=== Reify ===")
    pprint.pprintln(testReify)
    println("=== Build ===")
    pprint.pprintln(testBuild)
    println("=== Mapped ===")
    pprint.pprintln(testMapped)
    println("=== Formatted ===")
    pprint.pprintln(formatted)
    println("=== Widths ===")
    pprint.pprintln(widths)
    println("=== Padded ===")
    pprint.pprintln(padded)
    println("=== Folded ===")
    pprint.pprintln(testFold)
  }
}
