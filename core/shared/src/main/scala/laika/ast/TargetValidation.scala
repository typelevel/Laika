package laika.ast

sealed trait TargetValidation

object TargetValidation {
  case object ValidTarget                   extends TargetValidation
  case class InvalidTarget(message: String) extends TargetValidation

  case class RecoveredTarget(message: String, recoveredTarget: ResolvedInternalTarget)
      extends TargetValidation

}
