package gameanalyzer.wiki

import gameanalyzer.model.*
import gameanalyzer.WikiFormatter
import gameanalyzer.model.SkillCategory.ThroughputOptimization

object PageContentMaker {

  def defaultFormatter = WikiFormatter(
    iconSize = 24,
    showItemIcons = true,
    showBuildingIcons = true,
    showItemText = true
  )

  def smallIconFormatter = WikiFormatter(
    iconSize = 16,
    showItemIcons = true,
    showBuildingIcons = true,
    showItemText = true
  )

  def iconOnlyFormatter = WikiFormatter(
    iconSize = 24,
    showItemIcons = true,
    showBuildingIcons = true,
    showItemText = false
  )

  private def ingredientsList(
      items: Iterable[CountedItem],
      iconSize: Int = 24
  ): String =
    items.map(iconOnlyFormatter.formatCountedItem).mkString(" ")

  private def buildingDescription(b: Building): String =
    Seq(
      b.displayName,
      "Produces " + iconOnlyFormatter.formatCommaList(
        b.outputs.map(iconOnlyFormatter.formatCountedItem).toSeq
      )
    ).mkString("", ". ", ".")

  private def itemDescription(r: Item): String =
    Seq(
      Some(s"${r.category} item"),
      Some("Used in construction").filter(_ =>
        Buildings.allBuiltUsing(r).nonEmpty
      ),
      Some(
        Buildings
          .allConsuming(r)
          .map(b => smallIconFormatter.formatItem(b.mainOutput.item))
      )
        .filter(_.nonEmpty)
        .map(xs => s"Needed to craft ${smallIconFormatter.formatCommaList(xs)}")
    ).flatten.mkString("", ". ", ".")

  private def itemConstructionUsage(r: Item): String = {
    val buildings = Buildings.allBuiltUsing(r)
    if buildings.isEmpty then ""
    else {
      val header = s"== Use in Construction =="
      val table = WikiTables.mkTable(
        Seq("Building", "Construction Cost"),
        Seq("left", "left"),
        buildings.map { b =>
          Seq(
            defaultFormatter.formatBuilding(b),
            ingredientsList(b.cost.filter(_.qty > 0))
          )
        }
      )
      s"$header\n\n$table"
    }
  }

  private def itemConsumptionUsage(r: Item): String = {
    val buildings = Buildings.allConsuming(r)
    if buildings.isEmpty then ""
    else {
      val header = s"== Use in Recipes =="
      val table = WikiTables.mkTable(
        Seq("Building", "Output", "Inputs"),
        Seq("left", "left", "left"),
        buildings.map { b =>
          Seq(
            defaultFormatter.formatBuilding(b),
            ingredientsList(b.outputs),
            ingredientsList(b.inputs)
          )
        }
      )
      s"$header\n\n$table"
    }
  }

  private def itemProduction(r: Item): String = {
    val buildings = Buildings.allProducing(r)
    if buildings.isEmpty then ""
    else {
      val header = s"== Production =="
      val table = WikiTables.mkTable(
        Seq("Building", "Output", "Inputs"),
        Seq("left", "left", "left"),
        buildings.map { b =>
          Seq(
            defaultFormatter.formatBuilding(b),
            ingredientsList(b.outputs),
            ingredientsList(b.inputs)
          )
        }
      )
      s"$header\n\n$table"
    }
  }

  private def itemThroughputOptimization(
      r: Item
  ): Option[(String, String)] =
    Skill.values.collectFirst {
      case s: Skill
          if s.category == SkillCategory.ThroughputOptimization
            && s.affectedItems.contains(r) =>
        val table = WikiTables.mkTable(
          Seq("Skill Level", "Cost", "Description"),
          Seq("right", "right", "left"),
          s.makeRows(WikiFormatter()).map(_.productIterator.toSeq)
        )
        s.displayName -> table

    }

  private def itemSpecialization(r: Item): Option[(String, String)] =
    Skill.values.collectFirst {
      case s: Skill
          if s.category == SkillCategory.Specialization
            && s.affectedItems.contains(r) =>
        val table = WikiTables.mkTable(
          Seq("Skill Level", "Cost", "Description"),
          Seq("right", "right", "left"),
          s.makeRows(WikiFormatter()).map(_.productIterator.toSeq)
        )
        s.displayName -> table
    }

  private def itemStartingSupply(r: Item): Option[(String, String)] =
    Skill.starterKitResources.levels match {
      case rl: ItemLevels =>
        val relevantLevels = rl.levelsWithItem(r)

        val table = WikiTables.mkTable(
          Seq("Skill Level", "Cost", "Description"),
          Seq("right", "right", "left"),
          Skill.starterKitResources
            .makeRows(WikiFormatter())
            .filter(rowTuple => relevantLevels.contains(rowTuple._1))
            .map(_.productIterator.toSeq)
        )
        Some(Skill.starterKitResources.displayName -> table)
      case _ => None
    }

  def itemPage(r: Item): String = {
    val skillBlocks = Seq(
      itemThroughputOptimization(r),
      itemSpecialization(r),
      itemStartingSupply(r)
    ).flatten

    val skillSection =
      if skillBlocks.isEmpty then ""
      else {
        skillBlocks
          .map((title, table) => s"====[[$title]]====\n\n$table")
          .mkString("== Relevant Skills ==\n\n", "\n\n", "\n")
      }

    s"""
    ~{{Infobox simple
    ~| title = ${r.displayName}
    ~| image = ${r.name()}-96.png
    ~| description = ${itemDescription(r)}
    ~}}
    ~
    ~${itemConstructionUsage(r)}
    ~${itemConsumptionUsage(r)}
    ~${itemProduction(r)}
    ~$skillSection
    ~
    ~
    ~[[Category:Item]]
    ~""".stripMargin('~')
  }

  private def buildingCost(b: Building): String = {
    val header = s"== Building Cost =="
    val body = ingredientsList(b.cost.filter(_.qty > 0), iconSize = 32)
    s"$header\n\n$body"
  }

  private def buildingProduction(b: Building): String = {
    val header = s"== Production =="
    val buildingLink = defaultFormatter.formatBuilding(b)
    val outputs = ingredientsList(b.outputs)
    val inputs = ingredientsList(b.inputs)

    val table = WikiTables.mkTable(
      Seq("Building", "Output", "Inputs"),
      Seq("left", "left", "left"),
      Seq(
        Seq(buildingLink, outputs, inputs)
      )
    )
    s"$header\n\n$table"
  }

  def buildingPage(b: Building): String = {
    s"""
       ~{{Infobox simple
       ~| title = ${b.displayName}
       ~| description = ${buildingDescription(b)}
       ~}}
       ~
       ~${buildingCost(b)}
       ~${buildingProduction(b)}
       ~
       ~[[Category:Building]]
       ~${if b.minable then "[[Category:Mineable]]" else ""}
       ~""".stripMargin('~')
  }

  def skillTree(skilltree: SaveSkillTree): String = {
    val allNodes = skilltree.nodes
    val nonRootNodeIds = allNodes.flatMap(_.connectedNodeIds).toSet
    val rootNodes = allNodes.filterNot(n => nonRootNodeIds.contains(n.id))

    def recurse(parent: SaveSkillTreeNode, depth: Int): String = {
      val subNodes =
        allNodes.filter(n => parent.connectedNodeIds.contains(n.id))
      val subLines = subNodes.map(n => recurse(n, depth + 1))
      val prefix = "*" * depth
      val lines = s"$prefix [[${parent.name}]]" +: subLines
      lines.mkString("\n")
    }

    rootNodes
      .map(n => recurse(n, 1))
      .mkString("{{Tree list}}\n", "\n", "\n{{Tree list/end}}\n")
  }

  def skillPage(skill: Skill): String = {
    val content = skill.levels match {
      case SingleLevel(cost) =>
        s"""
           ~== Description ==
           ~
           ~${skill.displayName}
           ~
           ~== Cost ==
           ~
           ~$cost Points
           ~
           ~""".stripMargin('~')
      case ml: MultiLevels =>
        "== Levels ==\n\n" + WikiTables.mkTable(
          Seq("Level", "Cost", "Description"),
          Seq("left", "right", "left"),
          skill
            .makeRows(smallIconFormatter)
            .map(_.productIterator.toSeq)
        )
    }

    s"""
       ~$content
       ~
       ~== Unlocks ==
       ~${skill.connectedNodes
        .map(n => s"* [[${n.displayName}]]")
        .mkString("\n")}
       ~
       ~[[Category:Skill]]
       ~${skill.category match {
        case SkillCategory.Unlock =>
          "[[Category:Unlock Skill]]"
        case SkillCategory.LimitIncrease =>
          "[[Category:Limit Increase Skill]]"
        case SkillCategory.Specialization =>
          "[[Category:Specialization Skill]]"
        case SkillCategory.ThroughputOptimization =>
          "[[Category:Throughput Skill]]"
      }}
       ~
       ~""".stripMargin('~')
  }
}
