package incremental.model

import cats.data.{NonEmptyList, NonEmptySet}
import cats.kernel.Order

sealed trait Unlock {}

case object NoUnlock extends Unlock

case class UnlockParcelType(x: incremental.model.ParcelType) extends Unlock

case class UnlockAbility(description: String) extends Unlock

class UnlockBuildings(h: Building, t: Building*) extends Unlock {
  def seq: NonEmptyList[Building] = NonEmptyList.of(h, t*)
  def set: NonEmptySet[Building] = NonEmptySet.of(h, t*)(Order.by(_.ordinal))
}


