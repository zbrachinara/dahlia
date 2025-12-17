import fuselang.TestUtils.parseAst
import org.scalatest.funsuite.AnyFunSuite
import typechecker.IntegrityCheck

class EraserTests extends AnyFunSuite:
  test("EraseDeclaration"):
    val program = parseAst("decl x : bit<64> <H>;") 
    IntegrityCheck.erase(program)
    print(program)
//    assert(parseAst() == ???)
