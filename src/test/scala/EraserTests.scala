import fuselang.TestUtils.parseAst
import org.scalatest.funsuite.AnyFunSuite
import typechecker.IntegrityCheck

class EraserTests extends AnyFunSuite:
  test("EraseDeclaration"):
    val program = parseAst("decl x : bit<64> <H>;")
    IntegrityCheck.erase(program)
    val program_erased = parseAst("decl x : bit<64>;")
    assert(program == program_erased)

  test("EraseLet") :
    val program = parseAst("let x : bit<32> <L> = 1;")
    IntegrityCheck.erase(program)
    val program_erased = parseAst("let x : bit<32> = 1;")
    assert(program == program_erased)

  test("EraseWhile") :
    val program = parseAst("""
      while (false) {
        let x : bit<64> <L> = 1;
        for (let i = 0..10) unroll 5 {
          let y : bit<64> <L> = a[i];
          x + y;
        }
      }
    """)
    IntegrityCheck.erase(program)
    val program_erased = parseAst(
      """
              while (false) {
                let x : bit<64> = 1;
                for (let i = 0..10) unroll 5 {
                  let y : bit<64> = a[i];
                  x + y;
                }
              }
            """)
    assert(program == program_erased)

