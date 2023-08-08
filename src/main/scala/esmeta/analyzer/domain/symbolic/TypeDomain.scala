package esmeta.analyzer.domain.symbolic

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.ty.util.Stringifier.{*, given}
import esmeta.util.*
import esmeta.util.Appender
import esmeta.util.Appender.{*, given}
import esmeta.ir.IRElem
import esmeta.ir.Ref

object TypeDomain extends symbolic.Domain {
  // Key of the map should be concrete type but,
  // In ESMeta, Both concrete and abstract types are represented with AbsValue
  case class Elem(map: Map[AbsValue, Map[Ref, AbsValue]] = Map())
    extends Appendable

  lazy val Top: Elem = exploded("top abstract value")

  val Bot: Elem = Elem()

  // Currently we are not interested in concrete domain
  def alpha(xs: Iterable[ASymbolic]): Elem = ???

  def apply(map: Map[AbsValue, Map[Ref, AbsValue]]): Elem = Elem(map)

  given rule: Rule[Elem] = mkRule(true)

  extension (elem: Elem) {
    override def isBottom = elem.map.isEmpty

    def ⊑(that: Elem): Boolean = (elem, that) match
      case _ if elem.isBottom => true
      case _ if that.isBottom => false
      case (Elem(lmap), Elem(rmap)) => {
        val vs = lmap.keySet ++ rmap.keySet
        vs.forall(v =>
          val ls = lmap.getOrElse(v, Map())
          val rs = rmap.getOrElse(v, Map())
          val ids = ls.keySet ++ rs.keySet
          ids.forall(id =>
            ls.getOrElse(id, AbsValue.Bot) ⊑ rs.getOrElse(id, AbsValue.Bot),
          ),
        )
      }

    def ⊔(that: Elem): Elem = (elem, that) match
      case _ if elem.isBottom => that
      case _ if that.isBottom => elem
      case (Elem(lmap), Elem(rmap)) =>
        val newSymbolic = (for {
          x <- lmap.keySet ++ rmap.keySet
          ls = lmap.getOrElse(x, Map())
          rs = rmap.getOrElse(x, Map())
          ids = ls.keySet ++ rs.keySet
          v = (for {
            id <- ids
            l = ls.getOrElse(id, AbsValue.Bot)
            r = rs.getOrElse(id, AbsValue.Bot)
          } yield id -> (l ⊔ r)).toMap
        } yield x -> v).toMap
        Elem(newSymbolic)

    override def ⊓(that: Elem): Elem = (elem, that) match
      case _ if elem.isBottom || that.isBottom => Bot
      case (Elem(lmap), Elem(rmap)) =>
        val newSymbolic = (for {
          x <- lmap.keySet & rmap.keySet
          ls = lmap.getOrElse(x, Map())
          rs = rmap.getOrElse(x, Map())
          ids = ls.keySet & rs.keySet
          v = (for {
            id <- ids
            l = ls.getOrElse(id, AbsValue.Bot)
            r = rs.getOrElse(id, AbsValue.Bot)
          } yield id -> (l ⊓ r)).toMap
        } yield x -> v).toMap
        Elem(newSymbolic)

    def get(v: AbsValue): Map[Ref, AbsValue] =
      elem.map.get(v) match // maybe subtype check needed?
        case Some(m) => m
        case None    => Map()
  }

  private def mkRule(detail: Boolean): Rule[Elem] = (app, elem) =>
    if (!elem.isBottom) {
      val irStringifier = IRElem.getStringifier(detail, false)
      import irStringifier.given

      given Rule[Map[AbsValue, Map[Ref, AbsValue]]] = {
        given Rule[Map[Ref, AbsValue]] = unsortedMapRule(sep = ": ")
        unsortedMapRule(sep = ": ")
      }
      app >> elem.map >> LINE_SEP
    } else app >> "⊥"
}
