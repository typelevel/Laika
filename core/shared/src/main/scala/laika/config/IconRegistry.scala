package laika.config

import laika.api.config.{ ConfigEncoder, DefaultKey }
import laika.ast.{ Element, Icon }

/** Registers Icon AST elements for use with the `@:icon` directive and the `IconReference` AST element.
  */
class IconRegistry private (private val icons: Map[String, Icon]) {

  def getIcon(id: String): Option[Icon] = icons.get(id)

}

object IconRegistry {

  def apply(icons: (String, Icon)*): IconRegistry = new IconRegistry(icons.toMap)

  implicit val encoder: ConfigEncoder[IconRegistry] = ConfigEncoder.map[Element].contramap(_.icons)

  implicit val defaultKey: DefaultKey[IconRegistry] = DefaultKey(LaikaKeys.icons)

}
