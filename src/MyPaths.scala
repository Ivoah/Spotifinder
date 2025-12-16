package net.ivoah.spotifinder

import java.nio.file.{Path, Paths}

object MyPaths {
  private val os = System.getProperty("os.name").split(" ").head

  val dataDir: Path = os match {
    case "Mac" => Paths.get(System.getProperty("user.home"), "Library", "Application Support", "Spotifinder")
    case "Windows" => Paths.get(System.getenv("APPDATA"), "Spotifinder")
    case "Linux" => Paths.get(System.getProperty("user.home"), ".spotifinder")
  }

  val cacheDir: Path = os match {
    case "Mac" => Paths.get(System.getProperty("user.home"), "Library", "Caches", "net.ivoah.Spotifinder")
    case _ => dataDir.resolve("cache")
  }
}
