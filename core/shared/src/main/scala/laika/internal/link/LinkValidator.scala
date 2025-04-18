/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.internal.link

import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentDocument
import laika.ast.{
  DocumentCursor,
  GlobalLink,
  InternalTarget,
  InvalidSpan,
  Link,
  LocalLink,
  Path,
  RootCursor,
  RootElement,
  Span,
  Target,
  TargetValidation
}
import TargetValidation.*
import laika.config.{ LaikaKeys, LinkValidation, TargetFormats, Versions }
import laika.parse.SourceFragment
import cats.syntax.all.*
import laika.api.config.Config

/** Validates internal links based on the presence and configuration of the targets it points to.
  * A link target may be valid for all formats or just some, and it may point to a sub-directory
  * that is excluded from validation.
  *
  * The links returned by the validator might be a modified version of the original link.
  * While the `validate` method returns an Either and leaves error handling to the caller,
  * the `validateAndRecover` method replaces invalid links with an `InvalidSpan` node which is the most
  * common and most convenient approach.
  *
  * @author Jens Halm
  */
private[laika] class LinkValidator(
    cursor: DocumentCursor,
    findTargetFormats: Path => Option[TargetFormats]
) {

  private val outputFormat = cursor.root.outputContext.map(_.formatSelector)

  private val siteBaseURL = cursor.config.getOpt[String](LaikaKeys.siteBaseURL).toOption.flatten

  private val versionRoots = cursor.config.getOpt[Versions].toOption.flatten
    .map(vs => vs.allVersions.map(v => (Root / v.pathSegment / "doc").parent))
    .getOrElse(Nil)
    .toSet

  private val validationConfig =
    cursor.config.get[LinkValidation].getOrElse(LinkValidation.Local)

  private def isExcluded(path: Path, explicitExclusions: Seq[Path]): Boolean =
    versionRoots.exists(path.isSubPath) || explicitExclusions.exists(path.isSubPath)

  def validate(target: InternalTarget): TargetValidation = {

    val resolvedTarget = target.relativeTo(cursor.path)

    def isLocalTarget = resolvedTarget.relativePath.isInstanceOf[CurrentDocument]

    /*
    When a link target does exist, but does not support all of the output formats of the linking document,
    we can recover for the unsupported format by switching to an external link if:

    a) There is a value for `laika.siteBaseURL` in the transformer configuration
    b) `html` is one of the supported formats of the link target

    The final decision is still up to the caller of this method as not all type of targets can be switched
    to external linking.
    When rendering PDF for example, a text link can be switched to an external target, but an image cannot,
    as PDF readers cannot display remote images.
     */
    def attemptRecovery(message: String, targetFormats: TargetFormats): TargetValidation = {
      (targetFormats.contains("html"), siteBaseURL) match {
        case (true, Some(_)) =>
          RecoveredTarget(message, resolvedTarget.copy(internalFormats = targetFormats))
        case _               => InvalidTarget(message + validCondition)
      }
    }

    def validCondition: String = " unless html is one of the formats and siteBaseUrl is defined"

    def invalidRefMsg: String =
      s"cannot reference document '${resolvedTarget.relativePath.toString}'"

    def validateFormats(targetFormats: TargetFormats): TargetValidation = targetFormats match {
      case TargetFormats.All  => ValidTarget
      case TargetFormats.None => InvalidTarget(s"$invalidRefMsg as it is excluded from rendering")
      case TargetFormats.Selected(selectedFormats) =>
        (cursor.target.targetFormats, outputFormat) match {
          case (TargetFormats.None, _) =>
            ValidTarget // to be validated at point of inclusion by a directive like @:include
          case (_, Some(output)) if selectedFormats.contains(output) => ValidTarget
          case (_, Some(output))                                     =>
            attemptRecovery(
              s"document for output format $output $invalidRefMsg that does not support this format",
              targetFormats
            )
          case (TargetFormats.All, _)                                =>
            attemptRecovery(
              s"document for all output formats $invalidRefMsg with restricted output formats",
              targetFormats
            )
          case (TargetFormats.Selected(formats), _)                  =>
            val missingFormats = formats.filterNot(targetFormats.contains)
            if (missingFormats.isEmpty) ValidTarget
            else
              attemptRecovery(
                s"$invalidRefMsg that does not support some of the formats of this document (${
                    missingFormats.mkString(", ")
                  })",
                targetFormats
              )
        }
    }

    def validateTarget(explicitExclusions: Seq[Path]): TargetValidation =
      findTargetFormats(resolvedTarget.absolutePath) match {
        case None if isExcluded(resolvedTarget.absolutePath, explicitExclusions) =>
          val formats = cursor.root
            .selectTreeConfig(resolvedTarget.absolutePath.parent)
            .get[TargetFormats]
            .getOrElse(TargetFormats.All)
          validateFormats(formats)
        case None                                                                =>
          InvalidTarget(s"unresolved internal reference: ${resolvedTarget.relativePath.toString}")
        case Some(targetFormats)                                                 =>
          validateFormats(targetFormats)
      }

    validationConfig match {
      case LinkValidation.Off                     => ValidTarget
      case LinkValidation.Local if !isLocalTarget => ValidTarget
      case LinkValidation.Local                   => validateTarget(Nil)
      case LinkValidation.Global(excluded)        => validateTarget(excluded)
    }

  }

  /** Validates the specified link, verifying that the target exists and supports a matching set of target formats.
    * The returned link in case of successful validation might be a modified link with enhanced information for the
    * renderer, which might translate internal links to external links for some output formats.
    */
  def validate[L <: Link](link: L): Either[String, L] = {

    def validateTarget(target: Target): Either[String, L] = target match {
      case it: InternalTarget =>
        validate(it) match {
          case ValidTarget                       => Right(link)
          case InvalidTarget(error)              => Left(error)
          case RecoveredTarget(error, newTarget) =>
            link match {
              case gl: GlobalLink if gl.supportsExternalTargets =>
                Right(gl.withTarget(newTarget).asInstanceOf[L])
              case _                                            => Left(error)
            }
        }
      case _                  => Right(link)
    }

    link match {
      case gl: GlobalLink => validateTarget(gl.target)
      case ll: LocalLink  => validateTarget(InternalTarget(CurrentDocument(ll.refId)))
    }
  }

  /** Validates the specified link, verifying that the target exists and supports a matching set of target formats.
    * Performs the same checks as the `validate` method, but instead of returning potential errors in an Either
    * it replaces invalid links with instances of `InvalidSpan`.
    * Those types of AST nodes require access to the original source that produced the element which is either
    * obtained from a parser or from a directive combinator.
    */
  def validateAndRecover(link: Link, source: SourceFragment): Span =
    validate(link).valueOr(InvalidSpan(_, source))

}

private[laika] class TargetLookup(cursor: RootCursor) extends (Path => Option[TargetFormats]) {

  private lazy val docLookup: Map[Path, DocumentLookup] = cursor.target.allDocuments.map { doc =>
    (doc.path.withoutFragment, new DocumentLookup(doc.content, doc.config))
  }.toMap

  private lazy val staticLookup: Map[Path, TargetFormats] =
    cursor.target.staticDocuments.map(doc => (doc.path, doc.formats)).toMap

  def apply(path: Path): Option[TargetFormats] = {

    docLookup.get(path.withoutFragment).flatMap { lookup =>
      if (lookup.hasTarget(path)) Some(lookup.formats)
      else None
    }.orElse(staticLookup.get(path))
  }

  private class DocumentLookup(content: RootElement, config: Config) {

    private lazy val ids: Set[String] =
      content.collect {
        case e if e.hasId => e.options.id
      }.flatten.toSet

    lazy val formats: TargetFormats = config.get[TargetFormats].getOrElse(TargetFormats.All)

    def hasTarget(path: Path): Boolean = path.fragment match {
      case None           => true
      case Some(fragment) => ids.contains(fragment)
    }

  }

}
