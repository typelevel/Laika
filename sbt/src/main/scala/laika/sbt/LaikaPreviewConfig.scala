/*
 * Copyright 2012-2021 the original author or authors.
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

package laika.sbt

import com.comcast.ip4s._
import laika.preview.ServerConfig

import scala.concurrent.duration.FiniteDuration

/** Plugin configuration options specific to the preview server.
  *
  * @param port the port the server should run on (default 4242)
  * @param pollInterval the interval at which input file resources are polled for changes (default 3 seconds)
  * @param isVerbose whether each served page and each detected file change should be logged (default false)
  */
class LaikaPreviewConfig (val port: Port,
                          val host:Host,
                          val pollInterval: FiniteDuration,
                          val isVerbose: Boolean) {

  private def copy (newPort: Port = port,
                    newHost:Host = host,
                    newPollInterval: FiniteDuration = pollInterval,
                    newVerbose: Boolean = isVerbose): LaikaPreviewConfig =
    new LaikaPreviewConfig(newPort, newHost,newPollInterval, newVerbose)
  
  /** Specifies the port the server should run on (default 4242).
    */
  def withPort (port: Port): LaikaPreviewConfig = copy(newPort = port)

  def withHost(host:Host):LaikaPreviewConfig = copy(newHost = host)

  /** Specifies the interval at which input file resources are polled for changes (default 3 seconds).
    */
  def withPollInterval (interval: FiniteDuration): LaikaPreviewConfig = copy(newPollInterval = interval)

  /** Indicates that each served page and each detected file change should be logged to the console.
    */
  def verbose: LaikaPreviewConfig = copy(newVerbose = true)

}

/** Companion for preview server configuration instances.
  */
object LaikaPreviewConfig {

  /** A config instance populated with default values. */
  val defaults = new LaikaPreviewConfig(ServerConfig.defaultPort,ServerConfig.defaultHost, ServerConfig.defaultPollInterval, false)

}
