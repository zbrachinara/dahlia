package typechecker

import fuselang.common.Checker.{Checker, PartialChecker}
import fuselang.common.EnvHelpers.{ScopeManager, UnitEnv}
import fuselang.common.Syntax
import fuselang.common.Syntax.{CIf, CLet, CPar, CRange, CSeq, Command, Decl, Definition, ECast, Expr, FuncDef, Id, Prog, RecordDef, TSecLabeled, TVoid, Type}
import fuselang.typechecker.SecurityEnv

object TimingCheck {

  object Checker extends Checker:
    override type Env = SecurityEnv.Environment
    override val emptyEnv: Env = SecurityEnv.emptyEnv


  private object TimingCheck extends PartialChecker {

    override type Env = UnitEnv
    override val emptyEnv : UnitEnv = UnitEnv()
    
    override def checkC(cmd : Command)(implicit env : Env): Env = mergeCheckC({
      case (c @ CIf(cond, c1, c2), env) =>
        val condEnv = checkE(cond)
        ???
    })(cmd, env)
  }
}
