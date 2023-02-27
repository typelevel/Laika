package laika.helium

import laika.bundle.ExtensionBundle.LaikaDefaults

object LaikaVersion {

  lazy val value: String =
    LaikaDefaults.baseConfig.get[String]("laika.version").getOrElse("<unknown>")

}
