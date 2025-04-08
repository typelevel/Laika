/*
 * Copyright 2012-2025 the original author or authors.
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

package laika.internal.parse.hocon

import laika.api.config.ConfigValue.{
  ASTValue,
  ArrayValue,
  NullValue,
  ObjectValue,
  SimpleValue,
  StringValue
}
import laika.api.config.{ Config, ConfigEncoder, ConfigValue, Field, Key, ObjectConfig, Origin }
import cats.syntax.all.*
import laika.api.config.Config.ConfigResult
import laika.api.config.ConfigError.ResolverFailed

/** Interim structure that exists as a translation step between the `ConfigBuilderValue` ADT
  * that closely reflects the HOCON model right after parsing and the final `ConfigValue` ADT
  * that is the public API representing the fully resolved configuration values.
  *
  * The main purpose for this interim structure is to allow to defer the final resolution
  * of some values, mainly for three reasons:
  *
  * - Allow some values to depend on a parent `Config` which may change when a document is "re-homed" into
  *   a different tree
  *
  * - Allow errors to resolve on access only, preventing transformations to fail on values that are never accessed.
  *
  * - Allow the introduction of new public API that allows to create lazily evaluated configuration values
  *   that may also depend on other configuration values dynamically.
  */
private[laika] sealed trait FieldRef {

  def name: String

  def origin: Origin

  /* Navigates through nested structures to find the field with the specified key.
     Will return this instance for the root key and `None` when arriving at a leaf node
     with a key other than root.
   */
  def select(key: Key, context: ResolverContext): Option[ResolvedRef]

  protected def selectChild(
      name: String,
      subKey: Key,
      context: ResolverContext
  ): Option[ResolvedRef]

  def withName(name: String): FieldRef

}

/** A field reference that does not need access to the parent configuration
  * to be fully resolved.
  */
private[laika] sealed trait ResolvedRef extends FieldRef {

  type ValueType <: ConfigValue

  /** Defined only for types of values that do not require access to the config instance.
    * These types of values can be cached to avoid (potentially expensive) re-evaluations.
    */
  def cachedValue: ConfigResult[ValueType]

  def select(key: Key, context: ResolverContext): Option[ResolvedRef] =
    key.separateNextLevel.fold(Option(this)) { case (name, subKey) =>
      selectChild(name, subKey, context)
    }

  /** Performs a deep merge only if both, this value and the specified other value
    * are instances of `ObjectRef`.
    *
    * In all other cases the other object will take precedence and overwrite this value.
    */
  def merge(other: ResolvedRef): ResolvedRef = {
    (this, other) match {
      case (o1: ObjectRef, o2: ObjectRef) => o1.deepMerge(o2)
      case (_, c2)                        => c2
    }
  }

  def withName(name: String): ResolvedRef

}

private[laika] object ResolvedRef {

  def fromField(field: Field): ResolvedRef = field.value match {
    case s: SimpleValue => ScalarRef(field.key, field.origin, s)
    case o: ObjectValue =>
      ObjectRef(field.key, field.origin, o.values.map(fromField).toList)
    case a: ArrayValue  =>
      val fields = a.values.zipWithIndex.map { case (value, index) =>
        Field(index.toString, value, field.origin)
      }
      ArrayRef(field.key, field.origin, fields.map(fromField).toList)
    case ast: ASTValue  => ASTRef(field.key, field.origin, ast)
  }

  def fromRoot(root: ObjectValue, origin: Origin): ResolvedRef = fromField(Field("", root, origin))

}

private[laika] trait ResolvedLeafRef extends ResolvedRef {

  protected def selectChild(
      name: String,
      subKey: Key,
      context: ResolverContext
  ): Option[ResolvedRef] = None

}

/** A field reference that needs access to the parent configuration to be fully resolved.
  */
private[laika] sealed abstract class UnresolvedRef(val name: String, val origin: Origin)
    extends FieldRef {

  def select(key: Key, context: ResolverContext): Option[ResolvedRef] =
    key.separateNextLevel.fold(Option(resolve(context))) { case (name, subKey) =>
      selectChild(name, subKey, context)
    }

  /** Resolves the value based on the specified config instance.
    * The method always succeeds, potential errors will be encapsulated within the returned reference.
    */
  def resolve(context: ResolverContext): ResolvedRef

}

/**  Base trait for all fields that contain child nodes.
  */
private[laika] sealed trait ContainerRef extends ResolvedRef {

  def valueDescription: String

  def values: List[ResolvedRef]

  def createContainer(values: List[ConfigValue]): ConfigResult[ValueType]

  lazy val cachedValue: ConfigResult[ValueType] = {
    val (errors, cachedValues) = values.map(_.cachedValue).separate
    if (errors.isEmpty) createContainer(cachedValues)
    else {
      val msg =
        s"One or more errors resolving $valueDescription: ${errors.map(_.message).mkString(", ")}"
      Left(ResolverFailed(msg))
    }
  }

}

private[hocon] case class ScalarRef(name: String, origin: Origin, value: SimpleValue)
    extends ResolvedLeafRef {
  type ValueType = SimpleValue
  val cachedValue                         = Right(value)
  def withName(name: String): ResolvedRef = copy(name = name)
}

private[hocon] case class ASTRef(name: String, origin: Origin, value: ASTValue)
    extends ResolvedLeafRef {
  type ValueType = ASTValue
  val cachedValue                         = Right(value)
  def withName(name: String): ResolvedRef = copy(name = name)
}

private[hocon] case class InvalidRef(name: String, origin: Origin, error: String)
    extends ResolvedLeafRef {
  type ValueType = Nothing
  val cachedValue                         = Left(ResolverFailed(s"Invalid Field '$name': $error"))
  def withName(name: String): ResolvedRef = copy(name = name)

  override protected def selectChild(
      name: String,
      subKey: Key,
      context: ResolverContext
  ): Option[ResolvedRef] =
    Some(copy(error = s"Error in parent node of referenced object: $error"))

}

private[hocon] case class MissingOptionalRef(name: String, origin: Origin, substitutionKey: Key)
    extends ResolvedLeafRef {
  type ValueType = NullValue.type
  val cachedValue                         = Right(NullValue)
  def withName(name: String): ResolvedRef = copy(name = name)
}

private[laika] case class ArrayRef(name: String, origin: Origin, values: List[ResolvedRef])
    extends ContainerRef {

  type ValueType = ArrayValue

  val valueDescription = "elements of array value"

  def createContainer(children: List[ConfigValue]): ConfigResult[ValueType] =
    Right(ArrayValue(children))

  protected def selectChild(
      name: String,
      subKey: Key,
      context: ResolverContext
  ): Option[ResolvedRef] =
    values.find(_.name == name).flatMap(_.select(subKey, context))

  def withName(name: String): ResolvedRef = copy(name = name)
}

private[laika] case class ObjectRef(name: String, origin: Origin, values: List[ResolvedRef])
    extends ContainerRef {

  type ValueType = ObjectValue

  val valueDescription = "fields of object value"

  def createContainer(children: List[ConfigValue]): ConfigResult[ValueType] = {
    val fields = children.zip(values).map { case (child, field) =>
      Field(field.name, child, field.origin)
    }
    Right(ObjectValue(fields))
  }

  protected def selectChild(
      name: String,
      subKey: Key,
      context: ResolverContext
  ): Option[ResolvedRef] =
    values.find(_.name == name).flatMap(_.select(subKey, context))

  def deepMerge(other: ObjectRef): ObjectRef = {
    val resolvedFields = (this.values ++ other.values)
      .groupBy(_.name)
      .values
      .map(_.reduce(_.merge(_)))
      .toList
    ObjectRef(name, origin, resolvedFields.sortBy(_.name))
  }

  def withName(name: String): ResolvedRef = copy(name = name)

}

private[laika] object FieldRef {

  def container(
      fieldName: String,
      configOrigin: Origin,
      values: List[FieldRef],
      selectResolved: Boolean
  )(
      createContainer: (String, Origin, List[ResolvedRef]) => ResolvedRef
  ): FieldRef = {
    def filterOptional(resolvedValues: List[ResolvedRef]) =
      resolvedValues.filterNot(_.isInstanceOf[MissingOptionalRef]).sortBy(_.name)
    val resolved = values.collect { case r: ResolvedRef => r }
    if (resolved.size == values.size)
      createContainer(fieldName, configOrigin, filterOptional(resolved))
    else
      new UnresolvedRef(fieldName, configOrigin) {

        def resolve(context: ResolverContext): ResolvedRef = {
          val resolvedValues = values.map {
            case rr: ResolvedRef   => rr
            case ur: UnresolvedRef => ur.resolve(context)
          }
          createContainer(name, origin, filterOptional(resolvedValues))
        }

        protected def selectChild(
            name: String,
            subKey: Key,
            context: ResolverContext
        ): Option[ResolvedRef] = {
          if (selectResolved) resolve(context).selectChild(name, subKey, context)
          else values.find(_.name == name).flatMap(_.select(subKey, context))
        }

        def withName(name: String): FieldRef =
          container(name, configOrigin, values, selectResolved)(createContainer)
      }
  }

  def arrayRef(name: String, origin: Origin, values: List[FieldRef]): FieldRef =
    container(name, origin, values, selectResolved = false)(ArrayRef.apply)

  def objectRef(name: String, origin: Origin, values: List[FieldRef]): FieldRef =
    container(name, origin, values, selectResolved = false)(ObjectRef.apply)

  def concatRef(name: String, origin: Origin, values: List[FieldRef]): FieldRef = {
    container(name, origin, values, selectResolved = true) { (_, _, resolvedValues) =>
      def concat(v1: ResolvedRef, v2: ResolvedRef): ResolvedRef = {
        (v1, v2) match {
          case (o1: ObjectRef, o2: ObjectRef) => o1.deepMerge(o2)
          case (a1: ArrayRef, a2: ArrayRef)   => ArrayRef(name, origin, a1.values ++ a2.values)
          case (s1: ScalarRef, s2: ScalarRef) =>
            ScalarRef(name, origin, StringValue(s1.value.render + s2.value.render))
          case (error: InvalidRef, _)         => error
          case (_, error: InvalidRef)         => error
          case _                              =>
            val msg =
              "Invalid concatenation of values. It must contain either only objects, only arrays or only scalar values"
            InvalidRef(name, origin, msg)
        }
      }
      resolvedValues match {
        case Nil          => ScalarRef(name, origin, NullValue)
        case List(single) => single
        case multiple     => multiple.tail.foldLeft(multiple.head)(concat)
      }
    }
  }

  def mergedRef(name: String, origin: Origin, values: List[FieldRef]): FieldRef = {
    container(name, origin, values, selectResolved = true) { (_, _, resolvedValues) =>
      resolvedValues match {
        case Nil          => ScalarRef(name, origin, NullValue)
        case List(single) => single
        case multiple     => multiple.reduce(_.merge(_))
      }
    }
  }

  def deferred[T](fieldName: String, configOrigin: Origin, value: => T)(implicit
      encoder: ConfigEncoder[T]
  ): ResolvedRef =
    new ResolvedLeafRef {

      type ValueType = ConfigValue

      val name = fieldName

      val origin = configOrigin

      private lazy val encodedValue: ConfigValue = encoder(value)

      lazy val cachedValue: ConfigResult[ValueType] = Right(encodedValue)

      def withName(name: String): ResolvedRef = deferred(name, configOrigin, value)
    }

  def dependent[T](fieldName: String, configOrigin: Origin)(f: ResolverContext => ConfigResult[T])(
      implicit encoder: ConfigEncoder[T]
  ): FieldRef = new UnresolvedRef(fieldName, configOrigin) {
    def resolve(context: ResolverContext): ResolvedRef = f(context) match {
      case Left(error)  => InvalidRef(name, origin, error.message)
      case Right(value) => ResolvedRef.fromField(Field(fieldName, encoder(value), origin))
    }
    protected def selectChild(
        name: String,
        subKey: Key,
        context: ResolverContext
    ): Option[ResolvedRef] =
      resolve(context).selectChild(name, subKey, context)
    def withName(name: String): FieldRef               = dependent(name, configOrigin)(f)
  }

  def substitution(
      fieldName: String,
      configOrigin: Origin,
      substitutionRef: Key,
      optional: Boolean
  ): FieldRef = new UnresolvedRef(fieldName, configOrigin) {

    def resolve(context: ResolverContext): ResolvedRef = {

      def resolveSubstitution(currentConfig: Config): ResolvedRef = {
        currentConfig match {
          case foc: ObjectConfig =>
            foc.root
              .select(substitutionRef, context.withLookup(substitutionRef))
              .getOrElse(resolveSubstitution(foc.fallback))
              .withName(fieldName)
          case _ if optional     => MissingOptionalRef(fieldName, configOrigin, substitutionRef)
          case _                 =>
            val msg = s"Missing required reference: '$substitutionRef'"
            InvalidRef(fieldName, configOrigin, msg)
        }
      }

      if (context.lookups.contains(substitutionRef)) {
        val keys = context.lookups.toSeq.map(_.toString).sorted.mkString("'", "','", "'")
        InvalidRef(fieldName, configOrigin, s"Circular Reference involving paths $keys")
      }
      else resolveSubstitution(context.config)
    }

    protected def selectChild(
        name: String,
        subKey: Key,
        context: ResolverContext
    ): Option[ResolvedRef] =
      resolve(context).selectChild(name, subKey, context)
    def withName(name: String): FieldRef =
      substitution(name, configOrigin, substitutionRef, optional)
  }

}

private[laika] case class ResolverContext(config: Config, lookups: Set[Key] = Set.empty) {
  def withLookup(key: Key) = copy(lookups = lookups + key)
}
