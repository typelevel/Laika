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
import laika.parse.hocon.HoconParsers.{BuilderField, ConcatValue, ConfigBuilderValue, Field, ObjectBuilderValue, ObjectValue}

/**
  * @author Jens Halm
  */
object ConfigResolver {

  def resolve(root: ObjectBuilderValue): ObjectValue = {
    
    ???
  }
  
  def buildLookup(obj: ObjectBuilderValue): Map[Path, Seq[ConfigBuilderValue]] = ???
  
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
