name := "Spotifinder"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
  "com.lihaoyi" %% "requests" % "0.6.5",
  "com.typesafe.play" %% "play-json" % "2.9.0",
  "com.github.weisj" % "darklaf-core" % "2.5.4"
)

fork := true
javaOptions ++= Seq(
  "-Xdock:icon=src/main/resources/icon.png",
  s"-Xdock:name=$name"
)
