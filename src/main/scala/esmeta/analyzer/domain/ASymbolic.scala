package esmeta.analyzer.domain

import esmeta.ir.Ref

case class ASymbolic(map: Map[AValue, Map[Ref, AValue]] = Map()) {
  override def toString(): String = ??? // TODO
}
