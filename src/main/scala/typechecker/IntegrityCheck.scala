package typechecker

import fuselang.common.Checker.{Checker, PartialChecker}
import fuselang.common.EnvHelpers.{ScopeManager, UnitEnv}
import fuselang.common.Syntax
import fuselang.common.Syntax.{CPar, Decl, Definition, Expr, FuncDef, Id, Prog, RecordDef, TSecLabeled, TVoid, Type}

class IntegrityCheckEnv extends ScopeManager[IntegrityCheckEnv] {
  /**
   * Merge this environment with [[that]] for some abstract merge function.
   *
   * @assumes: this.getBoundIds == that.getBoundIds
   */
  override def merge(that: IntegrityCheckEnv): IntegrityCheckEnv = ???
}

object IntegrityCheck {

  object Checker extends Checker:
    override type Env = IntegrityCheckEnv
    override val emptyEnv: Env = IntegrityCheckEnv()



  private def walk_definitions(program : Prog) : Seq[Definition] =
    val Prog(includes, defs, _, decls, cmd) = program
    val allDefs = includes.flatMap(_.defs) ++ defs
    val topFunc = FuncDef(Id(""), decls, TVoid(), Some(cmd))
    allDefs ++ List(topFunc)

  // TODO should this also check or add annotations for timing information?
  /**
   * Checks that a program satisfies its security labels wrt information flow
   *
   * @param program A valid program, optionally containing security labels
   */
  def integrityCheck(program : Prog) : Unit = ???
  
  private object Eraser extends PartialChecker {

    override type Env = UnitEnv
    override val emptyEnv : UnitEnv = UnitEnv()
    
//    val partialZ
    private val partialExprCheck: PartialFunction[(Expr, Env), Env] = ???
//    override checkC(cmd : Command)(implicit env : Env) : Env = ???
    
  }

  private def strip_label(ty : Type) : Type = ty match
    case TSecLabeled(datatype, _, _) => datatype
    case x => x

  /**
   * Erases security labels from types.
   * All types containing security labels are unwrapped and replaced with the type inside,
   * which is expected to be and required to be a data type.
   *
   * @param program A valid program, optionally containing security labels
   */
  def erase(program : Prog) : Unit = {
    for (definition <- walk_definitions(program)) {
      definition match
        case fn @ FuncDef(_, args, _, body) =>
          fn.retTy = strip_label(fn.retTy)
          for (arg <- args) {
            arg.typ = strip_label(arg.typ)
          }
          
          ???
        case RecordDef(_, _) => ???
    }
  } // TODO crawl all declarations
}
