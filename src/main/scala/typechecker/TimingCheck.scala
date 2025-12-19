package typechecker

import fuselang.common.Checker.{Checker, PartialChecker}
import fuselang.common.EnvHelpers.{ScopeManager, UnitEnv}
import fuselang.common.Syntax
import fuselang.common.Syntax.{CIf, CLet, CPar, CRange, CSeq, Command, Decl, Definition, EArrAccess, EBinop, EBool, ECast, EInt, ERational, Expr, FuncDef, Id, Prog, RecordDef, SecurityLabel, TSecLabeled, TVoid, Type}
import fuselang.typechecker.SecurityEnv

object TimingCheck {

  private object TimingCheck extends PartialChecker {

    override type Env = SecurityEnv.Environment
    override val emptyEnv : Env = SecurityEnv.emptyEnv

    def level_of_expr_seq(expressions: Seq[Expr])(implicit env : Env) : SecurityLabel =
      (expressions map level_of_expr).foldLeft(SecurityLabel.Low){ (x, y) => x.max(y)}
    def level_of_expr(expr: Expr)(implicit env: Env) : SecurityLabel = expr match
      case EInt(_, _) | ERational(_) | EBool(_) => SecurityLabel.Low
      case EBinop(_, l, r) => level_of_expr(l).max(level_of_expr(r))
//      case EArrAccess(x, e) => env.get(x).get 
      case _ => ???

    override def checkC(cmd : Command)(implicit env : Env): Env = mergeCheckC({
      case (c @ CIf(cond, c1, c2), env) =>
        val condEnv = checkE(cond)
        ???
    })(cmd, env)
  }
}
