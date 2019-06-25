package laika.api.builder

import laika.bundle.ExtensionBundle

/** Base API for specifying configuration options that apply to all
  * kinds of operations (Parser, Renderer and Transformer).
  *
  * @author Jens Halm
  */
trait CommonBuilderOps {

  /** The type of the operation being configured by this instance
    */
  type ThisType

  /** Returns a new instance with the specified configuration.
    *
    * This method discards any previously specified
    * options. It is usually meant to be used when copying over
    * the configuration from a fully configured object to an
    * unconfigured one.
    */
  def withConfig (newConfig: OperationConfig): ThisType

  /** The current configuration for this instance.
    */
  protected def config: OperationConfig

  /** Returns a new instance with the specified extension bundles installed.
    * Features in the new bundles may override features in already installed bundles.
    *
    * Bundles are usually provided by libraries (by Laika itself or a 3rd-party extension library)
    * or as re-usable building blocks by application code.
    */
  def using (bundles: ExtensionBundle*): ThisType = withConfig(config.withBundles(bundles))

}
