import fuselang.TestUtils.parseAst
import org.scalatest.funsuite.AnyFunSuite
import typechecker.SecurityCheck

class SecurityTests extends AnyFunSuite:
  test("prohibited explicit flow"):
    val program = parseAst("let a: bit<32> <H> = 1; let b: bit<32> <L> = 2; b := a + 1;")
    assertThrows[RuntimeException] {
      SecurityCheck.check(program)
    }

  test("allowed flow low to high"):
    val program = parseAst("let a: bit<32> <H> = 1; let b: bit<32> <L> = 2; a := b + 1;")
    SecurityCheck.check(program)
