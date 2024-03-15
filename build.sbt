ThisBuild / scalaVersion := "3.3.1"

name := "ramjet"

lazy val `ramjet-core` = (project in file("modules/core"))

lazy val `ramjet-backend-storch` = (project in file("modules/backend-storch"))
  .dependsOn(`ramjet-core`)
  .settings(
    resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
    libraryDependencies ++= Seq(
      "dev.storch" %% "core" % "0.0-2dfa388-SNAPSHOT",
      "org.bytedeco" % "pytorch-platform" % "2.1.2-1.5.10"
    )
  )

lazy val `ramjet-exemples` = (project in file("modules/exemples"))
  .dependsOn(`ramjet-backend-storch`)
