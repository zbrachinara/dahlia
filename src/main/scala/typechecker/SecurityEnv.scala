package fuselang.typechecker

import scala.util.parsing.input.Position

import fuselang.Utils._
import Info._
import Gadgets._

import fuselang.common._
import ScopeMap._
import Syntax._
import Errors._
import CompilerError._
import EnvHelpers._

object SecurityEnv:

  val emptyEnv: Env = Env()(1)

  /**
   * A Security Env tracks:
   * - Variables, expressions and their security label
   * - For if statements, the number of `CSeq` encountered (to ensure that
   *   it is balanced at the logical timestep level)
   */
  case class Env(
    securityMap: ScopedMap[Id, SecurityLabel] = ScopedMap(),
    commandMap:  ScopedMap[Command, Int] = ScopedMap(),
  )(implicit val res: Int)
    extends ScopeManager[Env] {
    def update_label(key : Id, value : SecurityLabel) : Env =
      this.copy(securityMap = securityMap.add(key, value).getOrThrow(AlreadyBound(key)))

    def update_block(key : Command, value : Int) : Env =
      this.copy(commandMap = commandMap.add(key, value).getOrThrow(AlreadyBound(Id(key.toString))))

    def add(key: Id | Command, value: SecurityLabel | Int): Env =
      (key, value) match {
        case (id: Id, lbl: SecurityLabel) =>
          this.copy(securityMap =
            securityMap.add(id, lbl).getOrThrow(AlreadyBound(id))
          )

        case (cmd: Command, n: Int) =>
          this.copy(commandMap =
            commandMap.add(cmd, n).getOrThrow(AlreadyBound(Id(cmd.toString)))
          )
        case _ =>
          sys.error(s"Type mismatch in SecurityEnv.add: $key ↦ $value")
      }
      
    def addResource(name: Id, info: ArrayInfo): Env = ???

    /**
     * We want to enforce the fact that an Id corresponds to a security label and 
     * a command corresponds to an int, and the environment we merge with must have equal 
     * domain
     *
     * @requires: this.values of type command subsetOf next.values of type command
     * @requires: this.values of type Id subsetOf next.values of type Id
     * @requires: similar for keys but with labels and ints
     */
    override def merge(next: Env): Env = ???

    /**
     * Open a new scope and run commands in it. When the scope ends, new the
     * bindings bound in this scope are returned
     *
     * @param inScope   Commands executed with the inner scope.
     * @param count amount of logical timesteps the inner scope can take
     * @returns A new environment without the topmost scope and scopes
     *          containing bindings for physical resource and gadgets.
     */
    def withScope(resources: Int)(inScope: Env => Env): (Env, Map[Id, SecurityLabel], Map[Command, Int]) = ???

    def label_of_id(k : Id): SecurityLabel = securityMap.get(k).getOrThrow(Unbound(k))
    def index_of_block(block : Command): Int = commandMap.get(block).getOrThrow(
      RuntimeException("Internal error: Block expected to have been parsed, but was not found.")
    )

    /**
     * Get the resource associated with key if it is present.
     */
    def get(k: Id | Command): Option[SecurityLabel | Int] =
      k match {
        case id: Id => securityMap.get(id)
        case cmd: Command => commandMap.get(cmd)
      }

    def apply(k: Id | Command): SecurityLabel | Int =
      get(k).getOrThrow {
        k match {
          case id: Id => Unbound(id)
          case cmd: Command => Unbound(Id(cmd.toString))
        }
      }
  }