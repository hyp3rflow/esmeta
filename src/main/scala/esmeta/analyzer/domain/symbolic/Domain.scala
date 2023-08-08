package esmeta.analyzer.domain.symbolic

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.ir.*
import esmeta.state.*

trait Domain extends domain.Domain[ASymbolic] {
  def apply(map: Map[AbsValue, Map[Ref, AbsValue]]): Elem
}
