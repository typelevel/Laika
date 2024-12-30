import com.typesafe.tools.mima.core.{
  MissingClassProblem,
  ProblemFilter,
  ProblemFilters,
  ReversedMissingMethodProblem
}

object MimaFilters {

  val includeRefactoring: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[MissingClassProblem](
      "laika.helium.internal.builder.HeliumHeadDirectives"
    ),
    ProblemFilters.exclude[MissingClassProblem](
      "laika.helium.internal.builder.HeliumHeadDirectives$"
    ),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.StyleIncludes"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.StyleIncludes$"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.ScriptIncludes"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.ScriptIncludes$"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.ExternalCSS"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.ExternalCSS$"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.InternalCSS"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.InternalCSS$"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.InlineCSS"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.InlineCSS$"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.ExternalJS"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.ExternalJS$"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.InternalJS"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.InternalJS$"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.InlineJS"),
    ProblemFilters.exclude[MissingClassProblem]("laika.helium.internal.config.InlineJS$")
  )

  val internalDirectiveAPI: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[ReversedMissingMethodProblem](
      "laika.api.bundle.DirectiveBuilderContext.Directive"
    )
  )

}
