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

import com.comcast.ip4s.*
import laika.preview.ServerConfig

import scala.concurrent.duration.FiniteDuration

/** Plugin configuration options specific to the preview server.
  */
sealed abstract class LaikaPreviewConfig private {

  /** The port the server should run on (default 4242). */
  def port: Port

  /** The host the server should run on (default localhost). */
  def host: Host

  /** The interval at which input file resources are polled for changes (default 3 seconds). */
  def pollInterval: FiniteDuration

  /** Indicates whether each served page and each detected file change should be logged (default false). */
  def isVerbose: Boolean

  /** Specifies the port the server should run on (default 4242).
    */
  def withPort(port: Port): LaikaPreviewConfig

  /** Specifies the host the server should run on (default localhost).
    */
  def withHost(host: Host): LaikaPreviewConfig

  /** Specifies the interval at which input file resources are polled for changes (default 3 seconds).
    */
  def withPollInterval(interval: FiniteDuration): LaikaPreviewConfig

  /** Indicates that each served page and each detected file change should be logged to the console.
    */
  def verbose: LaikaPreviewConfig

}

/** Companion for creating preview server configuration instances.
  */
object LaikaPreviewConfig {

  private final case class Impl(
      port: Port,
      host: Host,
      pollInterval: FiniteDuration,
      isVerbose: Boolean
  ) extends LaikaPreviewConfig {
    override def productPrefix = "LaikaPreviewConfig"

    private def copy(
        newPort: Port = port,
        newHost: Host = host,
        newPollInterval: FiniteDuration = pollInterval,
        newVerbose: Boolean = isVerbose
    ): LaikaPreviewConfig =
      Impl(newPort, newHost, newPollInterval, newVerbose)

    def withPort(port: Port): LaikaPreviewConfig = copy(newPort = port)
    def withHost(host: Host): LaikaPreviewConfig = copy(newHost = host)

    def withPollInterval(interval: FiniteDuration): LaikaPreviewConfig =
      copy(newPollInterval = interval)

    def verbose: LaikaPreviewConfig = copy(newVerbose = true)
  }

  /** A config instance populated with default values. */
  val defaults: LaikaPreviewConfig = Impl(
    ServerConfig.defaultPort,
    ServerConfig.defaultHost,
    ServerConfig.defaultPollInterval,
    isVerbose = false
  )

}
