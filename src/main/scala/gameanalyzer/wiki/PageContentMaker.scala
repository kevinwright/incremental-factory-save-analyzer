package gameanalyzer.wiki

import gameanalyzer.model.{Building, Buildings, CountedResource, Resource}

extension (r: Resource) {
  def wikiIconLink(size: Int): String =
    s"[[File:${r.icon}|link=${r.name}|alt=${r.displayName}|${size}px]]"

  def wikiLink: String =
    if r == Resource.nullResource then ""
    else s"[[${r.name}|${r.displayName}]]"

  def wikiLinkWithIcon(size: Int): String =
    if r == Resource.nullResource then ""
    else s"${wikiIconLink(size)}[[${r.name}|${r.displayName}]]"
}

extension (b: Building) {
  def wikiLink: String =
    s"[[${b.name}|${b.displayName}]]"

  def wikiLinkWithIcon(size: Int): String = {
    val icon = b.mainOutput.resource.wikiIconLink(size)
    s"$icon[[${b.name}|${b.displayName}]]"
  }
}

extension (cr: CountedResource) {
  def wikiRender(iconSize: Int): String = {
    val qty =
      if cr.qty.floor == cr.qty
      then cr.qty.intValue.toString
      else cr.qty.toString

    val icon = cr.resource.wikiIconLink(iconSize)
    s"$qty × $icon"
  }
}

object PageContentMaker {

  private def commaList(xs: Seq[String]): String = {
    val result = xs.length match {
      case 0 => "Nothing"
      case 1 => xs.head
      case 2 => s"${xs.head} and ${xs.last}"
      case _ => xs.init.mkString("", ", ", s", and ${xs.last}")
    }
    result
  }

  private def ingredientsList(resources: Iterable[CountedResource]): String =
    resources.map(_.wikiRender(24)).mkString(" ")

  private def resourceDescription(r: Resource): String =
    Seq(
      Some(s"${r.category} resource"),
      Some("Used in construction").filter(_ =>
        Buildings.allBuiltUsing(r).nonEmpty
      ),
      Some(
        Buildings
          .allConsuming(r)
          .map(_.mainOutput.resource.wikiLinkWithIcon(16))
      )
        .filter(_.nonEmpty)
        .map(xs => s"Needed to craft ${commaList(xs)}")
    ).flatten.mkString("", ". ", ".")

  private def resourceConstructionUsage(r: Resource): String = {
    val buildings = Buildings.allBuiltUsing(r)
    if buildings.isEmpty then ""
    else {
      val header = s"== Use in Construction =="
      val table = WikiTables.mkTable(
        Seq("Building", "Construction Cost"),
        Seq("left", "left"),
        buildings.map { b =>
          Seq(
            b.wikiLinkWithIcon(size = 24),
            ingredientsList(b.cost.filter(_.qty > 0))
          )
        }
      )
      s"$header\n\n$table"
    }
  }

  private def resourceConsumptionUsage(r: Resource): String = {
    val buildings = Buildings.allConsuming(r)
    if buildings.isEmpty then ""
    else {
      val header = s"== Use in Recipes =="
      val table = WikiTables.mkTable(
        Seq("Building", "Output", "Inputs"),
        Seq("left", "left", "left"),
        buildings.map { b =>
          Seq(
            b.wikiLinkWithIcon(size = 24),
            ingredientsList(b.outputs),
            ingredientsList(b.inputs)
          )
        }
      )
      s"$header\n\n$table"
    }
  }

  private def resourceProduction(r: Resource): String = {
    val buildings = Buildings.allProducing(r)
    if buildings.isEmpty then ""
    else {
      val header = s"== Production =="
      val table = WikiTables.mkTable(
        Seq("Building", "Output", "Inputs"),
        Seq("left", "left", "left"),
        buildings.map { b =>
          Seq(
            b.wikiLinkWithIcon(size = 24),
            ingredientsList(b.outputs),
            ingredientsList(b.inputs)
          )
        }
      )
      s"$header\n\n$table"
    }
  }

  def resourcePage(r: Resource): String = {
    s"""
      ~{{Infobox simple
      ~| title = ${r.displayName}
      ~| description = ${resourceDescription(r)}
      ~}}
      ~
      ~${resourceConstructionUsage(r)}
      ~${resourceConsumptionUsage(r)}
      ~${resourceProduction(r)}
      ~
      ~[[Category:Resource]]
      ~""".stripMargin('~')
  }
}
