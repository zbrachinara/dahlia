package typechecker

import fuselang.common.Checker.PartialChecker
import fuselang.common.Syntax
import fuselang.common.Syntax.SecurityLabel.High
import fuselang.common.Syntax.{CIf, CSeq, Command, SecurityLabel}
import fuselang.typechecker.SecurityEnv


object TimeChecker extends PartialChecker {

    override type Env = SecurityEnv.Env
    override val emptyEnv: Env = SecurityEnv.emptyEnv

  // count the number of logical timesteps: for sequencing we loop through and recursively check
  // in case that there are logical timesteps nested inside commands, for if statements we count
  // each branch separately and then compare them, if equal we return the number otherwise we bail
    private def numTimeSteps(c: Command): Int = c match {
      case CSeq(cmds) =>
        cmds.foldLeft(0) { (t, cmd) =>
          val ct = numTimeSteps(cmd)
          if ct < 0 then -1 else t + 1
        }

      case CIf(_, c1, c2, _) =>
        val t1 = numTimeSteps(c1)
        val t2 = numTimeSteps(c2)
        if t1 == t2 then t1 else -1

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
