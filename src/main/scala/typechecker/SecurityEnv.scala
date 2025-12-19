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

  val emptyEnv: Environment = Env()(1)

  /**
   * A Security Env tracks:
   * - Variables, expressions and their security label
   * - For if statements, the number of `CSeq` encountered (to ensure that
   *   it is balanced at the logical timestep level)
   */
  sealed trait Environment
    extends ScopeManager[Environment]
      with Tracker[Id | Command, SecurityLabel | Int, Environment]:

    /**
     * Associate a gadget to the name of the physical resource it consumes.
     * Note that this DOES NOT associate it to the exact list of resources
     * consumed.
     * @param key Name of the gadget that consumes the resource.
     * @param value The name of the physical resource being consumed.
     *
     * For example, the call addGadget("V_A", "A") associates "V_A" to the
     * physical resource "A".
     */
    def add(key: Id | Command, value: SecurityLabel | Int): Environment

    /**
     * Get the value associated with the key.
     * @param id: tje key
     *
     */
    def apply(id: Id | Command): SecurityLabel | Int

    /**
     * Add physical resources to the environment.
     * @param id Name of the physical resource.
     * @param banks Number of banks available to this resource. All resources
     * are single dimensional. Multi-dimensional banks are implemented using
     * [[Gadget.MultiDimGadget]]
     */
    def addResource(name: Id, info: ArrayInfo): Environment

    /**
     * Merge this environment with [[next]] to create e' such that for each
     * physical resource id, e'.banks(id) <= this.banks(id) and e'.banks(id) <=
     * that.banks(id).
     * @requires: this.physicalResources subsetOf next.physicalResources
     * @requires: this.gadgetDefs subsetOf next.gadgetDefs
     */
    def merge(next: Environment): Environment

    /**
     * Open a new scope and run commands in it. When the scope ends, new the
     * bindings bound in this scope are returned
     *
     * @param inScope Commands executed with the inner scope.
     * @param timesteps Amount of timesteps that must be taken in the new scope
     * @returns A new environment without the topmost scope and scopes
     *          containing bindings for physical resource and gadgets.
     */
    def withScope(resources: Int)(
      inScope: Environment => Environment
    ): (Environment, Map[Id, SecurityLabel], Map[Command, Int])
  


  private case class Env(
                          securityMap: ScopedMap[Id, SecurityLabel] = ScopedMap(),
                          commandMap:  ScopedMap[Command, Int] = ScopedMap(),
                        )(implicit val res: Int)
    extends Environment {
    /**
     * Associate a gadget to the name of the physical resource it consumes.
     * Note that this DOES NOT associate it to the exact list of resources
     * consumed.
     *
     * @param gadget   Name of the gadget that consumes the resource.
     * @param resource The name of the physical resource being consumed.
     *
     *                 For example, the call addGadget("V_A", "A") associates "V_A" to the
     *                 physical resource "A".
     */
    override def add(key: Id | Command, value: SecurityLabel | Int): Environment =
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
      
    override def addResource(name: Id, info: ArrayInfo): Environment = ???

    /**
     * We want to enforce the fact that an Id corresponds to a security label and 
     * a command corresponds to an int, and the environment we merge with must have equal 
     * domain
     *
     * @requires: this.values of type command subsetOf next.values of type command
     * @requires: this.values of type Id subsetOf next.values of type Id
     * @requires: similar for keys but with labels and ints
     */
    override def merge(next: Environment): Environment = ???

    /**
     * Open a new scope and run commands in it. When the scope ends, new the
     * bindings bound in this scope are returned
     *
     * @param inScope   Commands executed with the inner scope.
     * @param count amount of logical timesteps the inner scope can take
     * @returns A new environment without the topmost scope and scopes
     *          containing bindings for physical resource and gadgets.
     */
    override def withScope(resources: Int)(inScope: Environment => Environment): (Environment, Map[Id, SecurityLabel], Map[Command, Int]) = ???

    /**
     * Get the resource associated with key if it is present.
     */
    override def get(k: Id | Command): Option[SecurityLabel | Int] =
      k match {
        case id: Id => securityMap.get(id)
        case cmd: Command => commandMap.get(cmd)
      }

    override def apply(k: Id | Command): SecurityLabel | Int =
      get(k).getOrThrow {
        k match {
          case id: Id => Unbound(id)
          case cmd: Command => Unbound(Id(cmd.toString))
        }
      }
  }