import fuselang.TestUtils.parseAst
import org.scalatest.funsuite.AnyFunSuite
import typechecker.TimeChecker
import typechecker.SecurityCheck

class TimingTests extends AnyFunSuite:
  test("ifStatementNoElse"):
    val program = parseAst(
      """
    let a: float[10] <H>;
    let b: float[10];
    a[0];
    ---
    if (a[0] == 1.0) {
      b[0];
      ---
      b[0];
    }
    ---
    a[1];
  """
    )
    SecurityCheck.check(program)
    assertThrows[RuntimeException] {
      TimeChecker.check(program)
    }

  test("ifStatementBalanced"):
    val program = parseAst(
      """
    let a: float[10] <H>;
    let b: float[10];
    a[0];
    ---
    if (a[0] == 1.0) {
      b[0];
      ---
      b[0];
    }
    else {
      b[0];
      ---
      b[0];
    }
    ---
    a[1];
  """
    )
    SecurityCheck.check(program)
    TimeChecker.check(program)

  test("ifStatementUnbalanced"):
    val program = parseAst(
      """
    let a: float[10] <H>;
    let b: float[10];
    a[0];
    ---
    if (a[0] == 1.0) {
      b[0];
      ---
      b[0];
    }
    else {
      b[0];
    }
    ---
    a[1];
  """
    )
    SecurityCheck.check(program)
    assertThrows[RuntimeException] {
      TimeChecker.check(program)
    }

  test("ifStatementLow"):
    val program = parseAst("""
      let a: bit<32> <L> = 1;
      if (a == 1) {
        a;
        ---
        a;
      }
      else {
        a;
      }
    """)
    SecurityCheck.check(program)
    TimeChecker.check(program)

  test("ifStatementNested1"):
    val program = parseAst(
      """
        let a: bit<32> <H> = 1;
        let b: bit<32> <L> = 2;
        if (a == 1) {
          a;
          ---
          a;
        }
        else {
          if (b == 2) {
             b;
          }
          else {
             b;
          }
          ---
          a;
        }
      """)
    SecurityCheck.check(program)
    TimeChecker.check(program)

  test("ifStatementNestedBad1"):
    val program = parseAst(
      """
       let a: bit<32> <H> = 1;
       let b: bit<32> <L> = 2;
       if (a == 1) {
         a;
         ---
         a;
       }
       else {
         if (b == 2) {
            b;
            ---
            a;
         }
         else {
            b;
         }
         ---
         a;
       }
     """)
    SecurityCheck.check(program)
    assertThrows[RuntimeException] {
      TimeChecker.check(program)
    }

  test("ifStatementDeepNestedBalanced"):
    val program = parseAst(
      """
        let h: bit<32> <H> = 1;
        let l: bit<32> <L> = 2;

        if (h == 1) {
          if (l == 2) {
            l;
            ---
            l;
          }
          else {
            l;
            ---
            l;
          }
          ---
          h;
        }
        else {
          if (l == 3) {
            l;
            ---
            l;
          }
          else {
            l;
            ---
            l;
          }
          ---
          h;
        }
      """
    )
    SecurityCheck.check(program)
    TimeChecker.check(program)

  test("ifStatementDeepNestedUnbalancedInner"):
    val program = parseAst(
      """
        let h: bit<32> <H> = 1;
        let l: bit<32> <L> = 2;

        if (h == 1) {
          if (l == 2) {
            l;
            ---
            l;
          }
          else {
            l;
          }
          ---
          h;
        }
        else {
          if (l == 3) {
            l;
            ---
            l;
          }
          else {
            l;
            ---
            l;
          }
          ---
          h;
        }
      """
    )
    SecurityCheck.check(program)
    assertThrows[RuntimeException] {
      TimeChecker.check(program)
    }



