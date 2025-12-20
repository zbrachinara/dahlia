package typechecker

import fuselang.common.Checker.{Checker, PartialChecker}
import fuselang.common.EnvHelpers.{ScopeManager, UnitEnv}
import fuselang.common.Syntax
import fuselang.common.Syntax.{CIf, CLet, CPar, CRange, CSeq, CUpdate, Command, Decl, Definition, EApp, EArrAccess, EArrLiteral, EBinop, EBool, ECast, EInt, EPhysAccess, ERational, ERecAccess, ERecLiteral, EVar, Expr, FuncDef, Id, Prog, RecordDef, SecurityLabel, TSecLabeled, TVoid, Type}
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
      case (CIf(cond, c1, c2), env) =>
        val cond_label = level_of_expr(cond)
        val env1 = env.copy(context = env.context.max(cond_label))
        env1.withScope({e =>
          checkC(c1)
          checkC(c2)
        })
        env

      case (CUpdate(lhs, rhs), env) =>
        val target_label = level_of_expr(lhs)
        val source_label = level_of_expr(rhs)
        if ! source_label.leq(target_label) then
          throw new RuntimeException(s"Assigned $rhs to $lhs, but it is more secure than $lhs")

        env

      case (CLet(id, typ, expr), env) =>
        val target_label = typ match
          case Some(TSecLabeled(_, label, _)) => label
          case _ => SecurityLabel.Low

        val env1 = env.update_id_label(id, target_label)

        // Check that flow is preserved
        expr match
          case Some(expr) =>
            val source_label = level_of_expr(expr)
            if ! source_label.leq(target_label) then
              throw new RuntimeException(s"Assigned $expr to $id, but it is more secure than $id")
          case _ =>

        env1
    })(cmd, env)
  }
}
