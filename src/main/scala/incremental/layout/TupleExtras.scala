package incremental.layout

import cats.syntax.FunctorTuple2Ops
import incremental.layout.table.Cell

import compiletime.ops.int.*
import scala.annotation.internal.Repeated
import scala.reflect.ClassTag
import scala.runtime.TupleMirror

////////////////
// Types

type HomogenousAux[H, T <: Tuple] = T match {
  case EmptyTuple => DummyImplicit
  case H *: t     => HomogenousAux[H, t]
  case _          => Nothing
}

type Homogenous[H] = [T <: Tuple] =>> HomogenousAux[H, T]

///////////////
// Extensions

extension [Tup <: Tuple](tup: Tup) {
  inline def toFixedList[
      N <: Tuple.Size[Tup] & Singleton
  ]: FixedList[N, Tuple.Union[Tup]] = FixedList.of[Tup, N](tup)
}
