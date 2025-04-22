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

package laika.api.config

import laika.api.config.Origin.Scope
import laika.ast.{ Element, Path }
import laika.internal.parse.hocon.ResolvedRef

/** The base trait for all configuration values.
  *
  * This data structure is quite similar to those found in common
  * JSON libraries (HOCON is a JSON superset after all).
  *
  * The only exception is one special type: the `ASTValue` which
  * can hold an instance of a document AST obtained from parsing
  * text markup.
  *
  * This can be useful in scenarios where substitution variables
  * in templates or markup want to refer to other AST elements
  * and include them into their AST tree as is.
  *
  * @author Jens Halm
  */
sealed trait ConfigValue extends Product with Serializable

object ConfigValue {

  /** Base trait for all simple configuration values. */
  sealed trait SimpleValue extends ConfigValue {
    def render: String
  }

  case object NullValue extends SimpleValue {
    val render: String = null
  }

  case class BooleanValue(value: Boolean) extends SimpleValue {
    def render: String = value.toString
  }

  case class DoubleValue(value: Double) extends SimpleValue {
    def render: String = value.toString
  }

  case class LongValue(value: Long) extends SimpleValue {
    def render: String = value.toString
  }

  case class StringValue(value: String) extends SimpleValue {
    def render: String = value
  }

  /** A value containing an AST element obtained from text markup or templates.
    *
    * Such a value can be used in scenarios where substitution variables
    * in templates or markup want to refer to other AST elements
    * and include them into their AST tree as is.
    */
  case class ASTValue(value: Element) extends ConfigValue

  case class ArrayValue(values: Seq[ConfigValue]) extends ConfigValue {
    def isEmpty: Boolean = values.isEmpty
  }

  case class ObjectValue(values: Seq[Field]) extends ConfigValue {

    def merge(other: ObjectValue): ObjectValue = {

      def mergeValues(cbv1: Field, cbv2: Field): Field = (cbv1, cbv2) match {
        case (Field(name, o1: ObjectValue, origin), Field(_, o2: ObjectValue, _)) =>
          Field(name, o1.merge(o2), origin)
        case (v1, _)                                                              => v1
      }

      val mergedFields = (values ++ other.values).groupBy(_.key).toSeq.map { case (_, fields) =>
        fields.reduce(mergeValues)
      }
      ObjectValue(mergedFields)
    }

    def toConfig: Config = {
      val origin =
        if (values.isEmpty) Origin.root
        else values.groupBy(_.origin).toSeq.maxBy(_._2.size)._1
      new ObjectConfig(ResolvedRef.fromRoot(this, origin))
    }

  }

}

/** A value tagged with its origin. */
case class Traced[T](value: T, origin: Origin)

/** The origin of a configuration value.
  *
  * Origins can be used to distinguish values from a specific Config
  * instance from those which were inherited from a fallback, which
  * might be relevant in scenarios where relative paths need to be
  * resolved.
  *
  * @param scope the scope of the containing config instance
  * @param path the virtual path of containing config instance in a document tree
  *             (not the key inside the configuration)
  * @param sourcePath the path in the file system this configuration originates from,
  *                   empty if it was constructed in memory
  */
case class Origin(scope: Scope, path: Path, sourcePath: Option[String] = None)

object Origin {

  val root: Origin = Origin(GlobalScope, Path.Root)

  sealed trait Scope
  case object GlobalScope    extends Scope
  case object TreeScope      extends Scope
  case object DocumentScope  extends Scope
  case object TemplateScope  extends Scope
  case object DirectiveScope extends Scope
}

/** A single field of an object value. */
case class Field(key: String, value: ConfigValue, origin: Origin = Origin.root)
