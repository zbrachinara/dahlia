import fuselang.TestUtils.parseAst
import org.scalatest.funsuite.AnyFunSuite
import typechecker.TimeChecker

class TimingTests extends AnyFunSuite:
  test("ifStatement"):
    val program = parseAst("let a: float[10] <H>; let b: float[10]; a[0]; --- if (a[0] == 1.0) {    b[0]; --- b[0]; } --- a[1];")
    TimeChecker.check(program)



