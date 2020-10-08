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

package laika.rewrite.link

import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast.{DocumentCursor, Image, InternalTarget, InvalidSpan, Link, Path, RelativePath, RootCursor, Span, SpanLink, Target}
import laika.config.LaikaKeys
import laika.parse.SourceFragment
import laika.rewrite.nav.TargetFormats

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
class LinkValidator (cursor: DocumentCursor, findTargetFormats: Path => Option[TargetFormats]) {

  private val siteBaseURL = cursor.config.getOpt[String](LaikaKeys.siteBaseURL).toOption.flatten

  private val excludedPaths = cursor.config.get[LinkConfig].getOrElse(LinkConfig.empty).excludeFromValidation.toSet
  private def excludeFromValidation (path: Path): Boolean = {

    def hasExcludedFlag (path: RelativePath): Boolean = cursor.root.tree.target.selectSubtree(path) match {
      case Some(tree) => !tree.config.get[Boolean](LaikaKeys.validateLinks).getOrElse(true)
      case None if path == CurrentTree => false
      case _ => hasExcludedFlag(path.parent)
    }

    excludedPaths.exists(path.isSubPath) || hasExcludedFlag(path.relative)
  }

  /** Validates the specified link, verifying that the target exists and supports a matching set of target formats.
    * The returned link in case of successful validation might be a modified link with enhanced information for the
    * renderer, which might translate internal links to external links for some output formats.
    */
  def validate (link: Link): Either[String, Link] = {

    def validateInternalTarget (internalTarget: InternalTarget): Either[String, Link] = {

      val target = internalTarget.relativeTo(cursor.path)
      
      def attemptRecovery (internalFormats: TargetFormats, msg: => String): Either[String, Link] = {
        (internalFormats.contains("html"), siteBaseURL, link) match {
          case (true, Some(_), sp: SpanLink) => Right(sp.copy(target = target.copy(
            internalFormats = internalFormats
          )))
          case _ => Left(msg)
        }
      }

      def findFormatConfig (path: Path): TargetFormats = cursor.root.tree.target.selectSubtree(path.relative) match {
        case Some(tree) => tree.config.get[TargetFormats].getOrElse(TargetFormats.All)
        case None if path == Root => TargetFormats.All
        case _ => findFormatConfig(path.parent)
      }
      
      def validCondition: String = "unless html is one of the formats and siteBaseUrl is defined"
      def invalidRefMsg: String = s"cannot reference document '${target.relativePath.toString}'"
      
      def validateFormats (targetFormats: TargetFormats): Either[String, Link] = targetFormats match {
        case TargetFormats.All => Right(link)
        case TargetFormats.None => Left(s"$invalidRefMsg as it is excluded from rendering")
        case TargetFormats.Selected(_) => cursor.target.targetFormats match {
          case TargetFormats.None => Right(link) // to be validated at point of inclusion by a directive like @:include
          case TargetFormats.All => 
            def msg = s"document for all output formats $invalidRefMsg with restricted output formats $validCondition"
            attemptRecovery(targetFormats, msg)
          case TargetFormats.Selected(formats) =>
            val missingFormats = formats.filterNot(targetFormats.contains)
            if (missingFormats.isEmpty) Right(link)
            else {
              def msg = s"$invalidRefMsg that does not support some of the formats of this document (${missingFormats.mkString(", ")}) $validCondition"
              attemptRecovery(targetFormats, msg)
            }
        }
      }

      findTargetFormats(target.absolutePath) match {
        case None if excludeFromValidation(target.absolutePath) => validateFormats(findFormatConfig(target.absolutePath.parent))
        case None => Left(s"unresolved internal reference: ${target.relativePath.toString}")
        case Some(targetFormats) => validateFormats(targetFormats)
      }
    }

    def validateTarget (target: Target): Either[String, Link] = target match {
      case it: InternalTarget => validateInternalTarget(it)
      case _                  => Right(link)
    }
    
    link match {
      case img: Image   => validateTarget(img.target)
      case sl: SpanLink => validateTarget(sl.target)
      case _            => Right(link)
    }
  }

  /** Validates the specified link, verifying that the target exists and supports a matching set of target formats.
    * Performs the same checks as the `validate` method, but instead of returning potential errors in an Either
    * it replaces invalid links with instances of `InvalidSpan`. 
    * Those types of AST nodes require access to the original source that produced the element which is either
    * obtained from a parser or from a directive combinator.
    */
  def validateAndRecover (link: Link, source: SourceFragment): Span = validate(link).fold(
    InvalidSpan(_, source),
    identity
  )
  
}

/** Temporary and incomplete workaround (does not validate target ids/fragments for now), 
  * until late link insertions get validated as part of the final rewrite step. */
private[laika] class TargetLookup (cursor: RootCursor) extends (Path => Option[TargetFormats]) {
  
  def apply (path: Path): Option[TargetFormats] = {

    def findRenderedDocument: Option[TargetFormats] =
      cursor.tree.target.selectDocument(path.relative).map { doc =>
        doc.config.get[TargetFormats].getOrElse(TargetFormats.All)
      }

    def findStaticDocument: Option[TargetFormats] =
      if (cursor.target.staticDocuments.contains(path.withoutFragment)) Some(TargetFormats.All) else None
    
    findRenderedDocument.orElse(findStaticDocument)
  }
  
}
