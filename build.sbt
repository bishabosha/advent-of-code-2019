val dottyVersion = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions += "-language:postfixOps",

    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % "test",
    libraryDependencies += "dev.zio" %% "zio-test" % "1.0.12",
    libraryDependencies += "org.typelevel" %% "spire" % "0.18.0-M1"
  )
