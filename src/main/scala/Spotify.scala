import play.api.libs.functional.syntax.toFunctionalBuilderOps

import java.util.Date
import play.api.libs.json._

import java.util.Base64
import java.security.MessageDigest
import java.net._
import java.io._
import scala.io._
import scala.util.{Random, Try}
import java.time.Instant

trait SpotifyItem {
  val id: String
  val name: String
  val uri: String
}

case class Spotify(private val client_id: String, scope: String = "") {
  private implicit val dateReads: Reads[Date] = Reads.dateReads("yyyy-MM-dd'T'HH:mm:ss'Z'")

  private implicit val tokenReads: Reads[Token] = (
    (JsPath \ "access_token").read[String] and
      (JsPath \ "token_type").read[String] and
      (JsPath \ "scope").read[String] and
      (JsPath \ "expires_in").read[Int] and
      (JsPath \ "refresh_token").read[String] and
      (JsPath \ "expires_on").readNullable[Long]
    ) { (access_token: String, token_type: String, scope: String, expires_in: Int, refresh_token: String, expires_on: Option[Long]) =>
    expires_on match {
      case Some(timestamp) => Token(access_token, token_type, scope, expires_in, refresh_token)(timestamp)
      case None => Token(access_token, token_type, scope, expires_in, refresh_token)()
    }
  }
  private implicit val tokenWrites: Writes[Token] = (
    (JsPath \ "access_token").write[String] and
      (JsPath \ "token_type").write[String] and
      (JsPath \ "scope").write[String] and
      (JsPath \ "expires_in").write[Int] and
      (JsPath \ "refresh_token").write[String] and
      (JsPath \ "expires_on").write[Long]
    ){token: Token => (token.access_token, token.token_type, token.scope, token.expires_in, token.refresh_token, token.expires_on)}
  private case class Token(access_token: String, token_type: String, scope: String, expires_in: Int, refresh_token: String)
                          (val expires_on: Long = Instant.now.getEpochSecond + expires_in) {
    override def toString: String = access_token
  }

  private var _token: Option[Token] = Try(Util._with(
    Source.fromFile(MyPaths.dataDir.resolve("token.json").toFile),
    s => Json.parse(s.mkString).as[Token]
  )).toOption
  private def token: Token = {
    _token match {
      case Some(token) if Instant.now.getEpochSecond < token.expires_on => token
      case Some(token) =>
        val new_token = refresh_token(token)
        _token = Some(new_token)
        cache(MyPaths.dataDir.resolve("token.json").toString, Json.prettyPrint(Json.toJson(token)), force = true)
        new_token
      case None =>
        val token = get_token()
        _token = Some(token)
        cache(MyPaths.dataDir.resolve("token.json").toString, Json.prettyPrint(Json.toJson(token)), force = true)
        token
    }
  }

  private def refresh_token(token: Token): Token = {
    try {
      val post = requests.post(
        "https://accounts.spotify.com/api/token",
        data = Map(
          "grant_type" -> "refresh_token",
          "refresh_token" -> token.refresh_token,
          "client_id" -> client_id
        )
      )
      Json.parse(post.text).as[Token]
    } catch {
      case e: requests.RequestFailedException if e.response.statusCode == 400 =>
        println("Error refreshing token")
        e.printStackTrace()
        get_token()
    }
  }

  private def get_token(): Token = {
    val code_verifier = Random.alphanumeric.take(64).mkString
    val code_challenge = Base64.getUrlEncoder.withoutPadding.encodeToString(MessageDigest.getInstance("SHA-256").digest(code_verifier.getBytes))

    java.awt.Desktop.getDesktop.browse(new URI(
      s"https://accounts.spotify.com/authorize?${Map(
        "client_id" -> client_id,
        "response_type" -> "code",
        "redirect_uri" -> "http://localhost:5122",
        "code_challenge_method" -> "S256",
        "code_challenge" -> code_challenge,
        "scope" -> scope
      ).map{case (key, value) => s"$key=${URLEncoder.encode(value, "UTF-8")}"}.mkString("&")}"
    ))
    val server = new ServerSocket(5122)
    val socket = server.accept()
    val in = new BufferedSource(socket.getInputStream)
    val out = new PrintWriter(socket.getOutputStream)
    val GET = in.getLines().next()
    out.write(
      """HTTP/1.1 200 OK
        |Content-Type: text/html
        |
        |<html>
        |    <head>
        |        <title>Success!</title>
        |    </head>
        |    <body>
        |        You can now close the webpage
        |    </body>
        |</html>
        |""".stripMargin
    )
    out.flush()
    socket.close()
    server.close()

    val code = raw"GET /\?code=(.*) HTTP".r.findFirstMatchIn(GET).get.group(1)

    val post = requests.post(
      "https://accounts.spotify.com/api/token",
      data = Map(
        "client_id" -> client_id,
        "grant_type" -> "authorization_code",
        "code" -> code,
        "redirect_uri" -> "http://localhost:5122",
        "code_verifier" -> code_verifier
      )
    )
    Json.parse(post.text).as[Token]
  }

  if (!MyPaths.cacheDir.toFile.exists) MyPaths.cacheDir.toFile.mkdirs()
  private def cache(filename: String, missing: => String, force: Boolean = false): String = {
    val cacheFile = MyPaths.cacheDir.resolve(filename).toFile
    if (cacheFile.exists && !force) {
      val source = io.Source.fromFile(cacheFile)
      val text = source.getLines().mkString
      source.close()
      text
    } else {
      val text = missing
      cacheFile.getParentFile.mkdirs()
      val writer = new java.io.FileWriter(cacheFile)
      writer.write(text)
      writer.close()
      text
    }
  }

  private def get(url: String): String = {
    cache(url.stripPrefix("https://") + ".json", requests.get(url, headers = Map("Authorization" -> s"Bearer $token")).text)
  }

  def clearCache(): Unit = {
    def delete(f: File): Unit = {
      if (f.isDirectory) f.listFiles.foreach(delete)
      f.delete()
    }
    delete(MyPaths.cacheDir.toFile)
  }

  def play(items: Seq[SpotifyItem]): Unit = {
    requests.put(
      "https://api.spotify.com/v1/me/player/play",
      data = JsObject(Seq(
        "uris" -> Json.toJson(items.map(_.uri))
      )).toString,
      headers = Map("Authorization" -> s"Bearer $token")
    )
  }
  def play(item: Track): Unit = play(Seq(item))
  def play(context: SpotifyItem, offset: Int): Unit = {
    requests.put(
      "https://api.spotify.com/v1/me/player/play",
      data = JsObject(Seq(
        "context_uri" -> JsString(context.uri),
        "offset" -> JsObject(Seq("position" -> JsNumber(offset)))
      )).toString,
      headers = Map("Authorization" -> s"Bearer $token")
    )
  }

  def queue(item: Track): Unit = {
    requests.post(
      "https://api.spotify.com/v1/me/player/queue",
      params = Map("uri" -> item.uri),
      headers = Map("Authorization" -> s"Bearer $token")
    )
  }

  private implicit val userReads: Reads[User] = Json.reads[User]
  object User {
    def apply(id: String) = new User(id)
    def unapply(user: User): Option[String] = Some(user.id)
    def fromURI(uri: String): User = new User(uri.split(':').last)
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

    val uri = s"spotify:user:$id"

    override def toString: String = name
  }

  private implicit val playlistReads: Reads[Playlist] = Json.reads[Playlist]
  object Playlist {
    def fromURI(uri: String): Playlist = {
      val id = uri.split(':').last
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

    val uri = s"spotify:playlist:$id"

    override def toString: String = name
  }

  private implicit val urlReads: Reads[URL] = (JsPath \ "url").read[String].map(new URL(_))
  private implicit val albumReads: Reads[Album] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "images").read[JsValue]
    ) { (id: String, name: String, artwork: JsValue) =>
    Album(id, name, artwork.as[Seq[URL]].headOption.getOrElse(getClass.getResource("missing.png")))
  }
  object Album {
    def fromURI(uri: String): Album = {
      val id = uri.split(':').last
      Json.parse(get(s"https://api.spotify.com/v1/albums/$id")).as[Album]
    }
  }
  case class Album(id: String, name: String, artwork: URL) extends SpotifyItem {
    val uri = s"spotify:album:$id"
  }

  private implicit val artistReads: Reads[Artist] = Json.reads[Artist]
  object Artist {
    def fromURI(uri: String): Artist = {
      val id = uri.split(':').last
      Json.parse(get(s"https://api.spotify.com/v1/artists/$id")).as[Artist]
    }
  }
  case class Artist(id: String, name: String) extends SpotifyItem {
    val uri = s"spotify:artist:$id"
  }

  private implicit val trackReads: Reads[Track] = Json.reads[Track]
  object Track {
    def fromURI(uri: String): Track = {
      val id = uri.split(':').last
      Json.parse(get(s"https://api.spotify.com/v1/tracks/$id")).as[Track]
    }
  }
  case class Track(id: String, name: String, album: Album, artists: Seq[Artist]) extends SpotifyItem {
    val uri = s"spotify:track:$id"
  }

  private implicit val playlistItemReads: Reads[PlaylistItem] = Json.reads[PlaylistItem]
  case class PlaylistItem(track: Track, added_by: User, added_at: Date) {
    override def toString: String = track.name
  }
}
