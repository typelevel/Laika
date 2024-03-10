package laika.sbt

import cats.data.Kleisli
import cats.effect.IO
import laika.ast.OutputContext
import laika.io.model.ParsedTree
import laika.theme.Theme.TreeProcessor

/** API shortcuts for adding tree processors to the the `laikaTreeProcessors` setting.
  *
  * Tree processors are effect-ful functions processing the document AST between parsing and rendering.
  *
  * One example would be generating a table of content based on inspecting the content of the tree and
  * insert it as the first document. You can also modify or delete content.
  *
  * Processors are usually specific per output format, therefore the API allows to specify the format
  * each processor should be applied to.
  *
  * @author Jens Halm
  */
trait TreeProcessors {

  class LaikaTreeProcessor(val delegate: OutputContext => TreeProcessor[IO])

  object LaikaTreeProcessor {

    /** Wraps a function that returns a tree processor depending on the output context passed.
      *
      * The returned instance can be added to the `laikaTreeProcessors` setting.
      */
    def apply(f: OutputContext => TreeProcessor[IO]): LaikaTreeProcessor =
      new LaikaTreeProcessor(f)

    /** Adds a function that processes the document tree between parsing and rendering,
      * to be executed only for the specified output format.
      *
      * The returned instance can be added to the `laikaTreeProcessors` setting.
      */
    def apply(f: TreeProcessor[IO], context: OutputContext): LaikaTreeProcessor =
      apply(rendererContext =>
        if (rendererContext == context) f else Kleisli.ask[IO, ParsedTree[IO]]
      )

  }

}
