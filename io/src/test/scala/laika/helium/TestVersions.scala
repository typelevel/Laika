package laika.helium

import laika.config.{ Version, Versions }

trait TestVersions {

  val versions: Versions = Versions
    .forCurrentVersion(Version("0.42.x", "0.42"))
    .withOlderVersions(
      Version("0.41.x", "0.41"),
      Version("0.40.x", "0.40").withFallbackLink("toc.html")
    )
    .withNewerVersions(
      Version("0.43.x", "0.43")
    )

}
