package incremental.model
package skills

object Skills {
  def affecting(r: Item): Seq[Skill] =
    Skill.values.filter(s => s.affectedItems.contains(r))
}
