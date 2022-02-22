package esmeta.ir

import esmeta.ir.util.*
import esmeta.util.Locational

// IR instructions
sealed trait Inst extends IRElem with Locational
object Inst extends Parser.From[Inst]

// normal instructions
sealed trait NormalInst extends Inst
case class IExpr(expr: Expr) extends NormalInst
case class ILet(lhs: Name, expr: Expr) extends NormalInst
case class IAssign(ref: Ref, expr: Expr) extends NormalInst
case class IDelete(ref: Ref) extends NormalInst
case class IPush(from: Expr, to: Expr, front: Boolean) extends NormalInst
case class IReturn(expr: Expr) extends NormalInst
case class IAssert(expr: Expr) extends NormalInst
case class IPrint(expr: Expr) extends NormalInst
case class INop() extends NormalInst

// special instructions
case class ISeq(insts: List[Inst]) extends Inst
case class IIf(cond: Expr, thenInst: Inst, elseInst: Inst) extends Inst
case class ILoop(kind: String, cond: Expr, body: Inst) extends Inst
case class ICall(lhs: Id, fexpr: Expr, args: List[Expr]) extends Inst