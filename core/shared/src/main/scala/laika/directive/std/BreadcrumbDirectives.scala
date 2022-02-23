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

package laika.directive.std

import cats.syntax.all._
import laika.ast._
import laika.directive.{Blocks, Templates}
import laika.parse.SourceFragment
import scala.annotation.tailrec

/** Provides the implementation for the standard breadcrumb directives.
  *
  * This includes the template and markup-block variants of this directive, 
  * which builds a navigation list from the root node of the input tree to the current document.
  * 
  * For full documentation see the section about the
  * [[https://planet42.github.io/Laika/07-reference/01-standard-directives.html#breadcrumb Breadcrumb Directive]]
  * in the manual.
  * 
  * @author Jens Halm
  */
object BreadcrumbDirectives {

  /** A block resolver that replaces itself with a navigation list from the root node of the input tree to the current document
    * during AST transformations.
    *
    * Serves as the implementation for the breadcrumb directive, but can also be inserted into the AST manually.
    */
  case class BreadcrumbBuilder (source: SourceFragment, options: Options = NoOpt) extends BlockResolver {

    type Self = BreadcrumbBuilder

    def resolve (cursor: DocumentCursor): Block = {

      val context = NavigationBuilderContext(
        refPath = cursor.path,
        itemStyles = Set(Style.breadcrumb.styles.head)
      )

      

      @tailrec
      def entriesFor(tree: TreeCursor,items:List[NavigationItem] = Nil) :List[NavigationItem] = {
        val title = tree.target.title.getOrElse(SpanSequence(tree.path.name))
        val item = context.newNavigationItem(title, tree.target.titleDocument, Nil, tree.target.targetFormats)
        tree.parent match {
          case None => item :: items
          case Some(parent) => entriesFor(parent,item ::items)
        }
      }

      val docEntry = {
        val title = cursor.target.title.getOrElse(SpanSequence(cursor.path.name))
        context.newNavigationItem(title, Some(cursor.target), Nil, cursor.target.targetFormats)
      }

      NavigationList(entriesFor(cursor.parent) :+ docEntry, Style.breadcrumb)
    }

    def withOptions (options: Options): BreadcrumbBuilder = copy(options = options)

    lazy val unresolvedMessage: String = "Unresolved breadcrumb builder"
  }

  /** Implementation of the `breadcrumb` directive for templates.
    */
  lazy val forTemplates: Templates.Directive  = Templates.create("breadcrumb") {

    import Templates.dsl._

    (cursor, Templates.dsl.source).mapN { case (cursor, src) =>
      TemplateElement(BreadcrumbBuilder(src).resolve(cursor))
    }
  }

  /** Implementation of the `breadcrumb` directive for block elements in markup documents.
    */
  lazy val forBlocks: Blocks.Directive  = Blocks.create("breadcrumb") {

    import Blocks.dsl._

    (cursor, Blocks.dsl.source).mapN { case (cursor, src) =>
      BreadcrumbBuilder(src).resolve(cursor)
    }
  }
  
}
