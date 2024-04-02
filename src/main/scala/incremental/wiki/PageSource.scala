package incremental.wiki

import incremental.model.*
import incremental.model.skills.Skill

trait PageSource[A]:
  extension (a: A)
    def title: String
    def redirectTitles: Seq[String]
    def mkContent: String

given PageSource[Item] with
  extension (x: Item)
    def title: String = x.displayName
    def redirectTitles: Seq[String] =
      Seq(x.name()).filterNot(_ equalsIgnoreCase title)
    def mkContent: String = PageContentMaker.itemPage(x)

given PageSource[Building] with
  extension (x: Building)
    def title: String = x.displayName
    def redirectTitles: Seq[String] =
      Seq(x.name()).filterNot(_ equalsIgnoreCase title)
    def mkContent: String = PageContentMaker.buildingPage(x)

given PageSource[Skill] with
  extension (x: Skill)
    def title: String = x.displayName
    def redirectTitles: Seq[String] =
      Seq(x.toString).filterNot(_ equalsIgnoreCase title)
    def mkContent: String = PageContentMaker.skillPage(x)

given PageSource[ParcelType] with
  extension (x: ParcelType)
    def title: String = x.displayName
    def redirectTitles: Seq[String] =
      Seq(x.name()).filterNot(_ equalsIgnoreCase title)
    def mkContent: String = PageContentMaker.parcelTypePage(x)
