package laika.rewrite.link

import laika.ast.{Element, Icon}
import laika.config.{ConfigEncoder, DefaultKey, LaikaKeys, NullValue}

/** Registers Icon AST elements for use with the `@:icon` directive and the `IconReference` AST element.
  */
case class IconRegistry (icons: Map[String, Icon])

object IconRegistry {

  def apply (icons: (String, Icon)*): IconRegistry = IconRegistry(icons.toMap)
  
  implicit val encoder: ConfigEncoder[IconRegistry] = ConfigEncoder.map[Element].contramap(_.icons)
  
  implicit val defaultKey: DefaultKey[IconRegistry] = DefaultKey(LaikaKeys.icons)
  
}
