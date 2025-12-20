package typechecker

import fuselang.common.Checker.{Checker, PartialChecker}
import fuselang.common.EnvHelpers.{ScopeManager, UnitEnv}
import fuselang.common.Syntax
import fuselang.common.Syntax.SecurityLabel.{High, Low}
import fuselang.common.Syntax.{CIf, CLet, CPar, CRange, CSeq, CUpdate, Command, Decl, Definition, EApp, EArrAccess, EArrLiteral, EBinop, EBool, ECast, EInt, EPhysAccess, ERational, ERecAccess, ERecLiteral, EVar, Expr, FuncDef, Id, Prog, RecordDef, SecurityLabel, TSecLabeled, TVoid, Type}
import fuselang.typechecker.SecurityEnv


object TimeChecker extends PartialChecker {

    override type Env = SecurityEnv.Env
    override val emptyEnv: Env = SecurityEnv.emptyEnv

    private def numTimeSteps(c: Command): Int = c match {
      case CSeq(c1) => c1.length - 1
      case _ => 0
    }

    override def checkC(cmd: Command)(implicit env: Env): Env = mergeCheckC({
      case (c @ CIf(cond, c1, c2, _), env) =>
        if c.secret || env.context == High then {
          val t1 = numTimeSteps(c1)
          val t2 = numTimeSteps(c2)
          if t1 != t2 then
            throw new RuntimeException(
              "secret-dependent branches have different amounts of logical time timesteps"
            )
            val env1 = env.copy(context = env.context.max(SecurityLabel.High))
            env1.withScope({ e =>
              checkC(c1)
              checkC(c2)
            })
        }
        env
      case (CSeq(c), env) => 
        c.foreach(checkC)
        env
    })(cmd, env)
  }
