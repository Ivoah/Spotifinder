name := "Spotifinder"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "requests" % "0.6.5",
  "com.typesafe.play" %% "play-json" % "2.9.0",
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
)

Compile / run / mainClass := Some("Spotifinder")
fork := true
javaOptions ++= Seq(
  "-Xdock:icon=src/main/resources/icon.png",
  s"-Xdock:name=$name"
)
