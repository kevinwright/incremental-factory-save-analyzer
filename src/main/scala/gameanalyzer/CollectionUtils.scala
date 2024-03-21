package gameanalyzer

import scala.collection.immutable.ListMap

object CollectionUtils {
  extension [K, V: Numeric](iter: Iterable[(K, V)]) {
    def sumValues: ListMap[K, V] = {
      iter
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.sum)
        .to(ListMap)
    }
  }
}
