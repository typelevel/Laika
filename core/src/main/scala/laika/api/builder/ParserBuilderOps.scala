package laika.api.builder

/** API for specifying configuration options that apply to all
  * kinds of operations that contain a parsing step (Parser and Transformer).
  *
  * @author Jens Halm
  */
trait ParserBuilderOps extends CommonBuilderOps {

  /**  Turns strict mode on for the target parser, switching off any
    *  features not part of the original markup syntax.
    *  This includes the registration of directives (custom tags), custom templates
    *  with directives, as well as configuration sections at the start of the document.
    *
    *  Technically it removes all `ExtensionBundle` instances which do not have
    *  the `useInStrictMode` flag set to true.
    */
  def strict: ThisType = withConfig(config.forStrictMode)

  /**  Enables all extensions that process raw content embedded into the host
    *  markup language.
    *  These are disabled by default as Laika is designed to render to multiple
    *  output formats from a single input document. With raw content embedded
    *  the markup document is tied to a specific output format.
    *
    *  Technically it activates all `ExtensionBundle` instances which have
    *  the `acceptRawContent` flag set to true.
    */
  def withRawContent: ThisType = withConfig(config.forRawContent)

}
