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
import laika.parse.hocon.HoconParsers.{ArrayBuilderValue, ArrayValue, BuilderField, ConcatValue, ConfigBuilderValue, ConfigValue, Field, NullValue, ObjectBuilderValue, ObjectValue, ResolvedBuilderValue, SelfReference, StringValue, SubstitutionValue}
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
    
    /*
    Concat String
    Concat Array
    Concat Object
    
    Self Reference
    Substitution as Self Reference
    Substitution of simple value
    Substitution in concatenated string
    Substitution as merged array
    Substitution as merged object
     */
    
    def deepMerge(o1: ObjectValue, o2: ObjectValue): ObjectValue =  {
      val resolvedFields = (o1.values ++ o2.values).groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
        case (name, values) => Field(name, values.reduce(merge(concat = false)))
      }
      ObjectValue(resolvedFields.sortBy(_.key))
    }

    def resolveValue(value: ConfigBuilderValue): ConfigValue = value match {
      case o: ObjectBuilderValue => resolveObject(o)
      case a: ArrayBuilderValue => ArrayValue(a.values.map(resolveValue))
      case r: ResolvedBuilderValue => r.value
      case c: ConcatValue => NullValue // TODO
      case SelfReference => NullValue // TODO
      case SubstitutionValue(ref, optional) => NullValue // TODO
    }
    
    def merge(concat: Boolean)(v1: ConfigValue, v2: ConfigValue): ConfigValue = {
      (v1, v2) match {
        case (o1: ObjectValue, o2: ObjectValue) => deepMerge(o1, o2)
        case (a1: ArrayValue, a2: ArrayValue) if concat => ArrayValue(a1.values ++ a2.values)
        case (s1: StringValue, s2: StringValue) if concat => StringValue(s1.value ++ s2.value) // TODO - ws from Concat Values
        case (c1, c2) if concat => NullValue // TODO - invalid combination of concat values
        case (_, c2) => c2
      }
    }
    
    def resolveObject(obj: ObjectBuilderValue): ObjectValue = {
      val resolvedFields = obj.values.groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
        case (path, values) => Field(path.name, values.map(resolveValue).reduce(merge(concat = false)))
      }
      ObjectValue(resolvedFields.sortBy(_.key))
    }
    
    resolveObject(expandPaths(root))
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
