package laika.execute

import laika.api.Render.Done
import laika.api.{Parse, Render, Transform}
import laika.io.RenderedTreeRoot

/**
  *  @author Jens Halm
  */
object TransformExecutor {

//  def execute[FMT] (op: Transform.Op[FMT]): String = {
//    val parseOp = Parse.Op(op.parsers, op.config, op.input, rewrite = true)
//    val doc = parseOp.execute
//    val renderOp = Render.Op(op.format, op.config, doc.content, op.output)
//    renderOp.execute
//  }
//
//  def execute[FMT] (op: Transform.TreeOp[FMT]): RenderedTreeRoot = {
//    val parseOp = Parse.TreeOp(op.parsers, op.config, op.input, rewrite = true)
//    val tree = parseOp.execute
//    val renderOp = Render.TreeOp(op.format, op.config, tree, op.output)
//    renderOp.execute
//  }
//
//  def execute[FMT] (op: Transform.MergeOp[FMT]): Done = {
//    val parseOp = Parse.TreeOp(op.parsers, op.config, op.input, rewrite = true)
//    val tree = parseOp.execute
//    val renderOp = Render.MergeOp(op.processor, op.config, tree, op.output)
//    renderOp.execute
//  }
  
}
