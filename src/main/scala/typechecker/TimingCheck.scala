package typechecker

import fuselang.common.Checker.{Checker, PartialChecker}
import fuselang.common.EnvHelpers.{ScopeManager, UnitEnv}
import fuselang.common.Syntax
import fuselang.common.Syntax.{CIf, CLet, CPar, CRange, CSeq, Command, Decl, Definition, ECast, Expr, FuncDef, Id, Prog, RecordDef, TSecLabeled, TVoid, Type}
import fuselang.typechecker.SecurityEnv

object TimingCheck {

  private object TimingCheck extends PartialChecker {

    override type Env = SecurityEnv.Environment
    override val emptyEnv : Env = SecurityEnv.emptyEnv

    override def checkE(expr: Expr)(implicit env: Env): Env = expr match
      case _ => ???
    
    override def checkC(cmd : Command)(implicit env : Env): Env = mergeCheckC({
      case (c @ CIf(cond, c1, c2), env) =>
        val condEnv = checkE(cond)
        ???
    })(cmd, env)
  }
}
