ThisBuild / scalaVersion := "3.3.1"

name := "ramjet"

lazy val `ramjet-core` = (project in file("modules/core"))

lazy val `ramjet-exemples` = (project in file("modules/exemples"))
  .dependsOn(`ramjet-core`)
