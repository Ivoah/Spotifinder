import java.io.File

object Paths {
  private val os = System.getProperty("os.name").split(" ").head

  val dataDir: File = os match {
    case "Mac" => new File (s"${System.getProperty("user.home")}/Library/Application Support/Spotifinder")
    case "Windows" => new File(s"${System.getenv("APPDATA")}/Spotifinder")
    case "Linux" => new File(s"${System.getProperty("user.home")}/.spotifinder")
  }

  val cacheDir = os match {
    case "Mac" => new File(s"${System.getProperty("user.home")}/Library/Caches/net.ivoah.Spotifinder")
    case _ => new File(s"$dataDir/cache")
  }
}
