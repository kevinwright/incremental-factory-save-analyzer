package incremental.model
package skills

object Skills {
  def ordered: Seq[Skill] =
    Skill.values.toSeq.sortBy(_.ordinal)
}
