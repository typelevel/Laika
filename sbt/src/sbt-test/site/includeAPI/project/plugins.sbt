{
  val pluginVersion = System.getProperty("plugin.version")
  if (pluginVersion == null)
    throw new RuntimeException(
      "The system property 'plugin.version' needs to be defined with scriptedLaunchOpts -D."
    )
  else addSbtPlugin("org.planet42" % "laika-sbt" % pluginVersion)
}
