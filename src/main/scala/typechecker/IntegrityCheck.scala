package typechecker

import fuselang.common.Syntax.{Decl, Definition, FuncDef, Id, Prog, RecordDef, TSecLabeled, TVoid, Type}

object IntegrityCheck {

  private def walk_definitions(program : Prog) : Iterable[Definition] =
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

  private def strip_label(ty : Type) : Type = ty match
    case TSecLabeled(datatype, _, _) => datatype
    case x => x
  /**
   * Erases security labels from types.
   * All types containing security labels are unwrapped and replaced with the type inside,
   * which is expected to be and required to be a data type.
   *
   * @param program A valid program, optionally containing security labels
   */
  def erase(program : Prog) : Unit = {
    for (definition <- walk_definitions(program)) {
      definition match
        case FuncDef(_, args, retTy, body) => {
          for (arg <- args) {
//            arg.typ = strip_label(arg.typ)
          }
          ???
        }
        case RecordDef(_, _) => ???
    }
  } // TODO crawl all declarations
}
