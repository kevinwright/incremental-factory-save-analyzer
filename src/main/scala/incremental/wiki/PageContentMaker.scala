package incremental.wiki

import incremental.model.*
import incremental.model.skills.*
import incremental.WikiFormatter
import incremental.model.skills.SkillCategory.ThroughputOptimization

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
          s.summarizeLevels(WikiFormatter())
            .map(sls => Seq(sls.level, sls.cost, sls.description))
            .toList
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
          s.summarizeLevels(WikiFormatter())
            .map(sls => Seq(sls.level, sls.cost, sls.description))
            .toList
        )
        s.displayName -> table
    }

  private def itemStartingSupply(item: Item): Option[(String, String)] =
    Skill.starterKitResources.levelDescriptors match {
      case _: SkillLevelDescriptor.ItemLevels =>
        val table = WikiTables.mkTable(
          Seq("Skill Level", "Cost", "Description"),
          Seq("right", "right", "left"),
          Skill.starterKitResources
            .summarizeLevels(WikiFormatter())
            .filter(sls => sls.provides.exists(_.item == item))
            .map(sls => Seq(sls.level, sls.cost, sls.description))
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

  def skillTree(): String = {
    val allSkills = Skill.values.toSeq
    val nonRootSkills = allSkills.flatMap(_.connectedNodes).toSet
    val rootSkills = allSkills.filterNot(nonRootSkills.contains)

    def recurse(parent: Skill, depth: Int): String = {
      val subNodes =
        allSkills.filter(parent.connectedNodes.contains)
      val subLines = subNodes.map(n => recurse(n, depth + 1))
      val prefix = "*" * depth
      val lines = s"$prefix [[${parent.displayName}]]" +: subLines
      lines.mkString("\n")
    }

    rootSkills
      .map(n => recurse(n, 1))
      .mkString("{{Tree list}}\n", "\n", "\n{{Tree list/end}}\n")
  }

  def skillPage(skill: Skill): String = {
    val content = skill.levelDescriptors match {
      case SkillLevelDescriptor.SingleLevel(cost) =>
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
      case _ =>
        "== Levels ==\n\n" + WikiTables.mkTable(
          Seq("Level", "Cost", "Description"),
          Seq("left", "right", "left"),
          skill
            .summarizeLevels(smallIconFormatter)
            .map(sls => Seq(sls.level, sls.cost, sls.description))
            .toList
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

  def parcelTypePage(parcelType: ParcelType): String = {
    val baseLimits = parcelType.baseLimits

    val buildingLimitLevels: Seq[(Int, ParcelLimits)] =
      (0 -> baseLimits) +:
        Skill.increaseBuildingLimit
          .summarizeLevels(defaultFormatter)
          .toList
          .map(sls =>
            sls.level -> baseLimits.withIncreaseBuildingLimit(sls.numericValue)
          )

    val connectionLevels: Seq[(Int, ParcelLimits)] =
      (0 -> baseLimits) +:
        Skill.increaseNodeConnections
          .summarizeLevels(defaultFormatter)
          .toList
          .map(sls =>
            sls.level -> baseLimits.withIncreaseNodeConnections(
              sls.numericValue
            )
          )

    val unlockSkills =
      Skill.values.toList.filter(
        _.unlocks match {
          case UnlockParcelType(pt) if pt == parcelType => true
          case _                                        => false
        }
      )

    val buildingLimitsTable = WikiTables.mkTable(
      Seq("Level", "Buildings", "Storage"),
      Seq("left", "right", "right"),
      buildingLimitLevels.map((level, limits) =>
        Seq(level, limits.buildings, limits.storage)
      )
    )

    val connectionLevelsTable = WikiTables.mkTable(
      Seq("Level", "Connections"),
      Seq("left", "right"),
      connectionLevels.map((level, limits) => Seq(level, limits.connections))
    )

    s"""
       ~=== Building Limits ===
       ~
       ~ [[${Skill.increaseBuildingLimit.displayName}]]
       ~
       ~ $buildingLimitsTable
       ~
       ~=== Connection Limits ===
       ~
       ~ [[${Skill.increaseNodeConnections.displayName}]]
       ~
       ~ $connectionLevelsTable
       ~
       ~${if unlockSkills.nonEmpty then "=== Associated Skills ===" else ""}
       ~
       ~${unlockSkills
        .map(n => s"* [[${n.displayName}]]")
        .mkString("\n")}
       ~
       ~[[Category:Parcel]]
       ~[[Category:Generated By A Bot]]
       ~
       ~""".stripMargin('~')
  }

  def simpleResearchPage(r: SimpleResearch): String = {
    val unlockStr = r.unlocks match {
      case NoUnlock => "nothing"
      case ub: UnlockBuildings =>
        defaultFormatter.formatCommaList(
          ub.seq.toList.map(defaultFormatter.formatBuilding)
        )
      case UnlockParcelType(pt) => s"[[${pt.displayName}]]"
      case UnlockAbility(str)   => str
    }

    s"""
       ~== Research: ${r.displayName} ===
       ~
       ~ unlocks $unlockStr
       ~
       ~[[Category:Research]]
       ~[[Category:Generated By A Bot]]
       ~
       ~""".stripMargin('~')
  }
}
