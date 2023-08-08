package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.analyzer.repl.*
import esmeta.cfg.*
import esmeta.ir.{Return, Func => IRFunc, Name, Param, Local, Ref}
import esmeta.error.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.Console.*
import scala.annotation.tailrec

case class ReturnEdge(np: NodePoint[Call], argRefs: List[Ref] = Nil)

/** abstract semantics */
class AbsSemantics(
  /** abstract states in each node point */
  var npMap: Map[NodePoint[Node], AbsState] = Map(),
  /** abstract states in each return point */
  var rpMap: Map[ReturnPoint, AbsRet] = Map(),
  /** abstract states right before calling functions */
  var callInfo: Map[NodePoint[Call], AbsState] = Map(),
  /** return edges */
  var retEdges: Map[ReturnPoint, Set[ReturnEdge]] = Map(),
  /** loop out edges */
  var loopOut: Map[View, Set[View]] = Map(),
  /** current control point */
  var curCp: Option[ControlPoint] = None,
) {

  /** a worklist of control points */
  val worklist: Worklist[ControlPoint] = QueueWorklist(npMap.keySet)

  /** the number of iterations */
  var iter: Int = 0

  /** count for each control point */
  var counter: Map[ControlPoint, Int] = Map()
  def getCount(cp: ControlPoint): Int = counter.getOrElse(cp, 0)

  /** RunJobs function */
  val runJobs = cfg.fnameMap("RunJobs")

  /** get return point of RunJobs */
  val runJobsRp = ReturnPoint(runJobs, View())

  /** get abstract return values and states of RunJobs */
  def finalResult: AbsRet = this(runJobsRp)

  /** set start time of analyzer */
  val startTime: Long = System.currentTimeMillis

  /** set of analyzed functions */
  def analyzedFuncs: Set[Func] =
    npMap.keySet.map(_.func) ++ rpMap.keySet.map(_.func)

  /** get return edges */
  def getRetEdges(rp: ReturnPoint): Set[ReturnEdge] =
    retEdges.getOrElse(rp, Set())

  /** lookup for node points */
  def apply(np: NodePoint[Node]): AbsState = npMap.getOrElse(np, AbsState.Bot)

  /** lookup for return points */
  def apply(rp: ReturnPoint): AbsRet = rpMap.getOrElse(rp, AbsRet.Bot)

  /** update internal map */
  def +=(pair: (NodePoint[Node], AbsState)): Unit =
    val (np, newSt) = pair
    val oldSt = this(np)
    if (!oldSt.isBottom && USE_REPL) REPL.merged = true
    if (!newSt.isBottom && !(newSt ⊑ oldSt))
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np

  /** loop transition for next views */
  def loopNext(view: View): View = view.loops match
    case LoopCtxt(loop, k) :: rest if IR_SENS =>
      view.copy(loops = LoopCtxt(loop, k + 1) :: rest)
    case _ => view

  /** loop transition for function enter */
  def loopEnter(view: View, loop: Branch): View =
    val loopView =
      if (IR_SENS)
        view.copy(
          loops = LoopCtxt(loop, 0) :: view.loops,
          intraLoopDepth = view.intraLoopDepth + 1,
        )
      else view
    loopOut += loopView -> (loopOut.getOrElse(loopView, Set()) + view)
    loopView

  /** loop transition for bases */
  def loopBase(view: View): View = view.loops match
    case LoopCtxt(loop, k) :: rest if IR_SENS =>
      view.copy(loops = LoopCtxt(loop, 0) :: rest)
    case _ => view

  /** loop transition for function exits */
  def loopExit(view: View): View = if (IR_SENS) {
    val views = loopOut.getOrElse(loopBase(view), Set())
    views.size match
      case 0 => error("invalid loop exit")
      case 1 => views.head
      case _ => exploded("loop is too merged.")
  } else view

  /** get entry views of loops */
  @tailrec
  final def getEntryView(view: View): View =
    if (!IR_SENS | view.intraLoopDepth == 0) view
    else getEntryView(loopExit(view))

  /** get abstract state of control points */
  def getState(cp: ControlPoint): AbsState = cp match
    case np: NodePoint[_] => this(np)
    case rp: ReturnPoint  => this(rp).state

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: String,
    detail: Boolean,
  ): String =
    val func = cp.func.name
    val cpStr = cp.toString(detail = detail)
    val k = setColor(color)(cpStr)
    cp match
      case np: NodePoint[_] =>
        val st = this(np).getString(detail = detail)
        s"""$k -> $st
           |${np.node}""".stripMargin
      case rp: ReturnPoint =>
        val st = this(rp).getString(detail = detail)
        s"""$k -> $st"""

  /** check reachability */
  def reachable(np: NodePoint[Node]): Boolean = !apply(np).isBottom
  def reachable(rp: ReturnPoint): Boolean = !apply(rp).isBottom

  /** conversion to string */
  override def toString: String = shortString

  /** conversion to short string */
  def shortString: String =
    s"- ${analyzedFuncs.size} functions are analyzed in $iter iterations."
}
