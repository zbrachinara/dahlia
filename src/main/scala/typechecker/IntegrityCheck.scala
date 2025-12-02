package typechecker

import fuselang.common.Syntax.Prog

object IntegrityCheck {
  // TODO should this also check or add annotations for timing information?
  /**
   * Checks that a program satisfies its security labels wrt information flow
   *
   * @param program A valid program, optionally containing security labels
   */
  def integrityCheck(program : Prog) : Unit = ???

  /**
   * Erases security labels from types.
   * All types containing security labels are unwrapped and replaced with the type inside,
   * which is expected to be and required to be a data type.
   *
   * @param program A valid program, optionally containing security labels
   */
  def erase(program : Prog) : Unit = ???
}
