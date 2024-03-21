package gameanalyzer

object MapUtils {
  private def sumValues[K, V: Numeric](seq: Seq[(K, V)]): ListMap[K, V] = {
    seq
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.sum)
      .to(ListMap)
  }
}
