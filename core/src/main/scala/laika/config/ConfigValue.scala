/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.config

import laika.ast.{Element, Path}
import laika.config.Origin.Scope

/**
  * @author Jens Halm
  */
sealed trait ConfigValue extends Product with Serializable

case class Traced[T] (value: T, origin: Origin)

case class Origin(scope: Scope, path: Path, sourcePath: Option[String] = None)

object Origin {
  
  val root: Origin = Origin(GlobalScope, Path.Root)
  
  sealed trait Scope
  case object GlobalScope extends Scope
  case object TreeScope extends Scope
  case object DocumentScope extends Scope
  case object TemplateScope extends Scope
  case object DirectiveScope extends Scope
}

sealed trait SimpleConfigValue extends ConfigValue {
  def render: String
}

case object NullValue extends SimpleConfigValue {
  val render: String = null
}
case class BooleanValue(value: Boolean) extends SimpleConfigValue {
  def render: String = value.toString
}
case class DoubleValue(value: Double) extends SimpleConfigValue {
  def render: String = value.toString
}
case class LongValue(value: Long) extends SimpleConfigValue {
  def render: String = value.toString
}
case class StringValue(value: String) extends SimpleConfigValue {
  def render: String = value
}

case class ASTValue(value: Element) extends ConfigValue

case class ArrayValue(values: Seq[ConfigValue]) extends ConfigValue {
  def isEmpty: Boolean = values.isEmpty
}
case class ObjectValue(values: Seq[Field]) extends ConfigValue {
  
  def merge (other: ObjectValue): ObjectValue = {
    
    def mergeValues(cbv1: Field, cbv2: Field): Field = (cbv1, cbv2) match {
      case (Field(name, o1: ObjectValue, origin), Field(_, o2: ObjectValue, _)) => Field(name, o1.merge(o2), origin)
      case (v1, _) => v1
    }

    val mergedFields = (values ++ other.values).groupBy(_.key).toSeq.map {
      case (_, fields) => fields.reduce(mergeValues)
    }
    ObjectValue(mergedFields)
  }
  
  def toConfig: Config = {
    val origin = if (values.isEmpty) Origin.root
      else values.groupBy(_.origin).toSeq.maxBy(_._2.size)._1
    new ObjectConfig(this, origin)
  }
}
case class Field(key: String, value: ConfigValue, origin: Origin = Origin.root)
