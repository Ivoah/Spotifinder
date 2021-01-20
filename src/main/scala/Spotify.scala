import play.api.libs.functional.syntax.toFunctionalBuilderOps

import java.util.Date
import play.api.libs.json._

import java.net.URL
import java.io.File

case class Spotify(private val client_id: String, private val client_secret: String) {
  private implicit val dateReads: Reads[Date] = Reads.dateReads("yyyy-MM-dd'T'HH:mm:ss'Z'")

  private lazy val token: String = {
    val auth = java.util.Base64.getEncoder.encodeToString(s"$client_id:$client_secret".getBytes)
    val post = requests.post(
      "https://accounts.spotify.com/api/token",
      data = Map("grant_type" -> "client_credentials"),
      headers = Map("Authorization" -> s"Basic $auth")
    )
    Json.parse(post.text)("access_token").as[String]
  }

  private val cacheDir = new File(s"${System.getProperty("user.home")}/Library/Caches/net.ivoah.Spotifinder")
  if (!cacheDir.exists) cacheDir.mkdirs()
  private def get(url: String): String = {
    val prefix = "https://" // Putting this inline breaks IntelliJ
    val cache = new File(s"$cacheDir/${url.stripPrefix(prefix)}.json")
    if (cache.exists) {
      val source = io.Source.fromFile(cache)
      val text = source.getLines().mkString
      source.close()
      text
    } else {
      val request = requests.get(url, headers = Map("Authorization" -> s"Bearer $token"))
      cache.getParentFile.mkdirs()
      val writer = new java.io.FileWriter(cache)
      writer.write(request.text)
      writer.close()
      request.text
    }
  }

  def clearCache(): Unit = {
    def delete(f: File): Unit = {
      if (f.isDirectory) f.listFiles.foreach(delete)
      f.delete()
    }
    delete(cacheDir)
  }

  trait SpotifyItem {
    val id: String
    val name: String
  }

  private implicit val userReads: Reads[User] = Json.reads[User]
  object User {
    def apply(id: String) = new User(id)
    def unapply(user: User): Option[String] = Some(user.id)
  }
  class User(val id: String) extends SpotifyItem {
    private def getPlaylists(url: String = s"https://api.spotify.com/v1/users/$id/playlists"): Seq[Playlist] = {
      val jsonObj = Json.parse(get(url))
      val playlists = jsonObj("items").as[Seq[Playlist]]
      jsonObj("next").asOpt[String] match {
        case Some(next) => playlists ++ getPlaylists(next)
        case None => playlists
      }
    }

    private val jsonObj: JsValue =
      if (id.nonEmpty) Json.parse(get(s"https://api.spotify.com/v1/users/$id"))
      else JsObject(Seq("display_name" -> JsString("None")))

    val name: String = jsonObj("display_name").as[String]
    lazy val playlists: Seq[Playlist] = if (id.nonEmpty) getPlaylists() else Seq()

    override def toString: String = name
  }

  private implicit val playlistReads: Reads[Playlist] = Json.reads[Playlist]
  object Playlist {
    def fromId(id: String): Playlist = {
      Json.parse(get(s"https://api.spotify.com/v1/playlists/$id")).as[Playlist]
    }
  }
  case class Playlist(id: String, name: String, description: String) extends SpotifyItem {
    private def getTracks(url: String = s"https://api.spotify.com/v1/playlists/$id/tracks"): Seq[PlaylistItem] = {
      val jsonObj = Json.parse(get(url))
      val tracks = jsonObj("items").asOpt[Seq[PlaylistItem]].getOrElse(Seq())
      (jsonObj \ "next").asOpt[String] match {
        case Some(next) => tracks ++ getTracks(next)
        case None => tracks
      }
    }
    lazy val tracks: Seq[PlaylistItem] = getTracks()

    override def toString: String = name
  }

  private implicit val urlReads: Reads[URL] = (JsPath \ "url").read[String].map(new URL(_))
  private implicit val albumReads: Reads[Album] = (
//    (JsPath \ "id").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "images").read[JsValue]
    ) { (name: String, artwork: JsValue) =>
    Album(name, artwork.as[Seq[URL]].headOption.getOrElse(getClass.getResource("missing.png")))
  }
  case class Album(name: String, artwork: URL)

  private implicit val artistReads: Reads[Artist] = Json.reads[Artist]
  case class Artist(name: String)

  private implicit val trackReads: Reads[Track] = Json.reads[Track]
  case class Track(name: String, album: Album, artists: Seq[Artist])

  private implicit val playlistItemReads: Reads[PlaylistItem] = Json.reads[PlaylistItem]
  case class PlaylistItem(track: Track, added_by: User, added_at: Date) {
    override def toString: String = track.name
  }
}
