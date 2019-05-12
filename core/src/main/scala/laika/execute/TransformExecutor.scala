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

package laika.execute

import laika.api.Render.Done
import laika.api.{Parse, Render, Transform}
import laika.io.RenderedTreeRoot

/**
  *  @author Jens Halm
  */
object TransformExecutor {

  def execute[FMT] (op: Transform.Op[FMT]): String = {
    val parseOp = Parse.Op(op.parsers, op.config, op.input, rewrite = true)
    val doc = parseOp.execute
    val renderOp = Render.Op(op.format, op.config, doc.content, op.output)
    renderOp.execute
  }

  def execute[FMT] (op: Transform.TreeOp[FMT]): RenderedTreeRoot = {
    val parseOp = Parse.TreeOp(op.parsers, op.config, op.input, rewrite = true)
    val tree = parseOp.execute
    val renderOp = Render.TreeOp(op.format, op.config, tree, op.output)
    renderOp.execute
  }

  def execute[FMT] (op: Transform.MergeOp[FMT]): Done = {
    val parseOp = Parse.TreeOp(op.parsers, op.config, op.input, rewrite = true)
    val tree = parseOp.execute
    val renderOp = Render.MergeOp(op.processor, op.config, tree, op.output)
    renderOp.execute
  }
  
}
