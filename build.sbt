ThisBuild / version := "0.2"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-source:3.0-migration")

ThisBuild / assemblyMergeStrategy := {
  case PathList("module-info.class") => MergeStrategy.discard
  case x if x.endsWith("/module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}

lazy val root = (project in file("."))
  .settings(
    name := "Spotifinder",
    idePackagePrefix := Some("net.ivoah.spotifinder"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      "com.lihaoyi" %% "requests" % "0.7.0",
      "com.typesafe.play" %% "play-json" % "2.10.1",
      "com.github.weisj" % "darklaf-core" % "2.7.3"
    ),
    assembly / assemblyOutputPath := file("Spotifinder.jar"),
    fork := true,
    javaOptions ++= Seq(
      "-Xdock:icon=src/main/resources/icon.png",
      s"-Xdock:name=$name"
    )
  )

