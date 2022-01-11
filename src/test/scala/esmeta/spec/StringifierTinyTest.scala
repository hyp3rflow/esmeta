package esmeta.spec

import esmeta.util.BaseUtils.*
import esmeta.spec.*
import Symbol.*

class StringifierTinyTest extends SpecTest {
  val name: String = "specStringifierTest"

  // test helper
  def test[T <: SpecElem](desc: String)(cases: (T, String)*): Unit =
    check(
      desc,
      cases.foreach { case (input, expected) =>
        val result = input.toString
        if (result != expected) {
          println(s"[FAILED: Symbol]")
          println(s" - result  : $result")
          println(s" - expected: $expected")
          assert(result == expected)
        }
      },
    )

  // registration
  def init: Unit = {
    // pre-defined values
    val ntArgs = List(
      NtArg(NtArg.Kind.True, "Await"),
      NtArg(NtArg.Kind.False, "Yield"),
      NtArg(NtArg.Kind.Pass, "For"),
    )
    val nt: Nonterminal = Nonterminal("Identifier", Nil, false)
    val symbols = List(Terminal("{"), Terminal("}"))

    test("Symbol")(
      Terminal("{") -> "`{`",
      Nonterminal(
        "Identifier",
        ntArgs,
        true,
      ) -> "Identifier[+Await, ~Yield, ?For]?",
      Nonterminal("Identifier", Nil, false) -> "Identifier",
      ButNot(nt, List(nt)) -> "Identifier but not Identifier",
      Lookahead(
        true,
        List(symbols, symbols),
      ) -> "[lookahead < {`{` `}`, `{` `}`}]",
      Lookahead(
        false,
        List(symbols, symbols),
      ) -> "[lookahead <! {`{` `}`, `{` `}`}]",
      Empty -> "[empty]",
      NoLineTerminator -> "[no LineTerminator here]",
      Unicode("LT") -> "<LT>",
      UnicodeAny -> "<UnicodeAny>",
      UnicodeIdStart -> "<UnicodeIdStart>",
      UnicodeIdContinue -> "<UnicodeIdContinue>",
      UnicodeLeadSurrogate -> "<UnicodeLeadSurrogate>",
      UnicodeTrailSurrogate -> "<UnicodeTrailSurrogate>",
      NotCodePoint -> "<NotCodePoint>",
      CodePoint -> "<CodePoint>",
      HexLeadSurrogate -> "<HexLeadSurrogate>",
      HexTrailSurrogate -> "<HexTrailSurrogate>",
      HexNonSurrogate -> "<HexNonSurrogate>",
      NonUnicodeModeDecimalEscape -> "<NonUnicodeModeDecimalEscape>",
    )

    test("NtArg")(
      NtArg(NtArg.Kind.True, "Await") -> "+Await",
      NtArg(NtArg.Kind.False, "Yield") -> "~Yield",
      NtArg(NtArg.Kind.Pass, "Wait") -> "?Wait",
    )

    test("NtArg.Kind")(
      NtArg.Kind.True -> "+",
      NtArg.Kind.False -> "~",
      NtArg.Kind.Pass -> "?",
    )

    test("RhsCond")(
      RhsCond("Hello", true) -> "[+Hello]",
      RhsCond("Bye", false) -> "[~Bye]",
    )

    val rhsCond: RhsCond = RhsCond("Yield", true)
    val rhs1: Rhs = Rhs(Some(rhsCond), symbols, None)
    val rhs2: Rhs = Rhs(Some(rhsCond), symbols, Some("Identifier"))
    val lhs = Lhs("lhs", List("Yield", "Await", "In"))
    val prod_str1 =
      """lhs[Yield, Await, In] :: one of
  [+Yield] `{` `}`
  [+Yield] `{` `}` #Identifier
"""
    val prod_str2 =
      """lhs[Yield, Await, In] :
  [+Yield] `{` `}`
  [+Yield] `{` `}` #Identifier
"""
    val prod1 = Production(lhs, Production.Kind.Lexical, true, List(rhs1, rhs2))
    val prod2 = Production(lhs, Production.Kind.Normal, false, List(rhs1, rhs2))

    test("Rhs")(
      rhs1 -> "[+Yield] `{` `}`",
      rhs2 -> "[+Yield] `{` `}` #Identifier",
    )

    test("Lhs")(
      lhs -> "lhs[Yield, Await, In]",
    )

    test("Production")(
      prod1 -> prod_str1,
      prod2 -> prod_str2,
    )

    test("Production.Kind")(
      Production.Kind.Normal -> ":",
      Production.Kind.Lexical -> "::",
      Production.Kind.NumericString -> ":::",
    )

    test("Grammar")(
      Grammar(List(prod1, prod2), List(prod1)) ->
        ("""########################################
# Productions
########################################
""" + prod_str1 + "\n" + prod_str2 + "\n" +
          """########################################
# Productions for Web
########################################
""" + prod_str1),
    )

    // EXAMPLE test("Inst")(
    //   IExpr(EINum(4)) -> "4i",
    // )
  }

  init
}
