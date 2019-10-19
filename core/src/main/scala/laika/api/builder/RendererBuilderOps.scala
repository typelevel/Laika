package laika.api.builder

import laika.ast.{Element, MessageLevel}
import laika.bundle.ExtensionBundle
import laika.factory.RenderFormat

/** API for specifying configuration options that apply to all
  * kinds of operations that contain a rendering step (Renderer and Transformer).
  *
  * @author Jens Halm
  */
trait RendererBuilderOps[FMT] extends CommonBuilderOps {

  protected def renderFormat: RenderFormat[FMT]

  /**  Specifies a custom render function that overrides one or more of the default
    *  renderers for the output format this instance uses.
    *
    *  This method expects a partial function that takes a formatter and the element
    *  to render. It will then be invoked for each element it is defined at.
    *
    *  Simple example for customizing the HTML output for emphasized text, adding a specific
    *  style class:
    *
    *  {{{
    *  val transformer = Transformer.from(Markdown).to(HTML).rendering {
    *    case (fmt, Emphasized(content, opt)) => fmt.element("em", opt, content, "class" -> "big")
    *  }.build
    *  }}}
    */
  def rendering (customRenderer: PartialFunction[(FMT, Element), String]): ThisType = using(new ExtensionBundle {
    override val useInStrictMode: Boolean = true
    override val themes = Seq(renderFormat.Theme(customRenderer = customRenderer))
  })

  /**  Specifies the minimum required level for a system message
    *  to get included into the output by this renderer.
    */
  def withMessageLevel (level: MessageLevel): ThisType = withConfig(config.copy(minMessageLevel = level))

  /**  Renders without any formatting (line breaks or indentation).
    *  Useful when storing the output in a database for example.
    */
  def unformatted: ThisType = withConfig(config.copy(renderFormatted = false))

}
