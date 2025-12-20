package typechecker

import fuselang.common.Checker.{Checker, PartialChecker}
import fuselang.common.EnvHelpers.{ScopeManager, UnitEnv}
import fuselang.common.Syntax
import fuselang.common.Syntax.{CIf, CLet, CPar, CRange, CSeq, Command, Decl, Definition, EApp, EArrAccess, EArrLiteral, EBinop, EBool, ECast, EInt, EPhysAccess, ERational, ERecAccess, ERecLiteral, EVar, Expr, FuncDef, Id, Prog, RecordDef, SecurityLabel, TSecLabeled, TVoid, Type}
import fuselang.typechecker.SecurityEnv

object TimingCheck {

  private object TimingCheck extends PartialChecker {

    override type Env = SecurityEnv.Env
    override val emptyEnv : Env = SecurityEnv.emptyEnv

    private def level_of_expr_seq(expressions: Seq[Expr])(implicit env : Env) : SecurityLabel =
      (expressions map level_of_expr).foldLeft(SecurityLabel.Low){ (x, y) => x.max(y)}
    private def level_of_expr(expr: Expr)(implicit env: Env) : SecurityLabel = expr match
      case EInt(_, _) | ERational(_) | EBool(_) => SecurityLabel.Low
      case EBinop(_, l, r) => level_of_expr(l).max(level_of_expr(r))
      case EArrAccess(x, e) => env.label_of_id(x).max(level_of_expr_seq(e))
      // TODO figure out the deal with EPhysAccess
      case _ : EPhysAccess => throw new RuntimeException("Internal Error: Should be unreachable at this stage")
      case EArrLiteral(e) => level_of_expr_seq(e)
      case ERecAccess(e, x) => env.label_of_id(x).max(level_of_expr(e)) 
      case ERecLiteral(e) => level_of_expr_seq(e.values.toSeq)
      case EApp(_, e) => level_of_expr_seq(e)
      case EVar(x) => env.label_of_id(x)
      case ECast(e, _) => level_of_expr(e)

    override def checkC(cmd : Command)(implicit env : Env): Env = mergeCheckC({
      case (c @ CIf(cond, c1, c2), env) =>
        val cond_label = level_of_expr(cond)
        ???
    })(cmd, env)
  }
}
