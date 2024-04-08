package incremental.layout
package table

case class ColumnDef private (
    title: Content,
    titleAlignment: Alignment,
    alignment: Alignment
) {
  def withTitle(newTitle: Content): ColumnDef = new ColumnDef(
    newTitle,
    titleAlignment,
    alignment
  )
}

object ColumnDef {
  def apply[T](
      title: T,
      titleAlignment: Alignment = Alignment.auto,
      alignment: Alignment = Alignment.auto
  )(using conv: ConvertsToContent[T]) = new ColumnDef(
    conv.convert(title),
    titleAlignment,
    alignment
  )
}
