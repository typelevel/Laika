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

package laika.parse.hocon

import laika.ast.Path
import laika.parse.hocon.HoconParsers.{BuilderField, ConcatValue, ConfigBuilderValue, ConfigValue, Field, ObjectBuilderValue, ObjectValue, ResolvedBuilderValue}
import laika.collection.TransitionalCollectionOps._

import scala.collection.mutable

/**
  * @author Jens Halm
  */
object ConfigResolver {

  def resolve(root: ObjectBuilderValue): ObjectValue = {
    
    val activePaths = mutable.Set.empty[Path]
    val resolvedPaths = mutable.Map.empty[Path, ConfigValue]
    val invalidPaths = mutable.Map.empty[Path, String]
    
    def loop(obj: ObjectBuilderValue): ObjectValue = {
      val resolvedFields = obj.values.groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
        case (path, Seq(ResolvedBuilderValue(v))) => Field(path.name, v)
      }
      ObjectValue(resolvedFields.sortBy(_.key))
    }
    
    loop(expandPaths(root))
  }
  
  /** Expands all flattened path expressions to nested objects.
    * 
    * ```
    * { a.b.c = 7 }
    * ```
    * 
    * will become
    * 
    * ```
    * { a = { b = { c = 7 }}}
    * ```
    */
  def expandPaths(obj: ObjectBuilderValue, path: Path = Path.Root): ObjectBuilderValue = {
    
    def expandValue(value: ConfigBuilderValue, child: Path): ConfigBuilderValue = value match {
      case o: ObjectBuilderValue => expandPaths(o, child)
      case c: ConcatValue => c.copy(
        first = expandValue(c.first, child),
        rest = c.rest.map(part => part.copy(value = expandValue(part.value, child)))
      )
      case other => other
    }
    
    val expandedFields = obj.values.map { field =>
      field.key.components match {
        case name :: Nil => 
          field.copy(
            key = path / name, 
            value = expandValue(field.value, path / name)
          )
        case name :: rest =>
          field.copy(
            key = path / name, 
            value = expandPaths(ObjectBuilderValue(Seq(BuilderField(Path(rest), field.value))), path / name)
          )
      }
    }
    obj.copy(values = expandedFields)
  }
  
}
