package typechecker

import fuselang.common.Checker.{Checker, PartialChecker}
import fuselang.common.EnvHelpers.{ScopeManager, UnitEnv}
import fuselang.common.Syntax
import fuselang.common.Syntax.{CLet, CPar, CRange, Command, Decl, Definition, ECast, Expr, FuncDef, Id, Prog, RecordDef, TSecLabeled, TVoid, Type}

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

    private def strip_label(ty: Type): Type = ty match
      case TSecLabeled(datatype, _, _) => datatype
      case x => x

    override def checkC(cmd : Command)(implicit env : Env) : Env = mergeCheckC({
      case (c @ CRange(_, _, _, _, _, _), env) =>
        c.castType = c.castType map strip_label
        env
      case (c @ CLet(_, _, expr), env) => 
        c.typ = c.typ map strip_label
        expr.map(checkE).getOrElse(env) // From the superclass
    })(cmd, env)

    override def checkE(expr: Expr)(implicit env: UnitEnv): UnitEnv = mergeCheckE({
      case (e @ ECast(einner, _), env) =>
        e.typ = e.typ map strip_label
        checkE(einner) // From the superclass
    })(expr, env)
    
  }

  /**
   * Erases security labels from types.
   * All types containing security labels are unwrapped and replaced with the type inside,
   * which is expected to be and required to be a data type.
   *
   * @param program A valid program, optionally containing security labels
   */
  def erase(program : Prog) : Unit = {
    Eraser.check(program)
  } // TODO crawl all declarations
}
