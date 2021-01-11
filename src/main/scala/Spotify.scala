import play.api.libs.functional.syntax.toFunctionalBuilderOps

import java.util.Date
import play.api.libs.json._

import java.text.SimpleDateFormat

case class Spotify(client_id: String, client_secret: String) {
  private implicit val dateReads: Reads[Date] = Reads.dateReads("yyyy-MM-dd'T'HH:mm:ss'Z'")

  private lazy val token: String = {
    val auth = java.util.Base64.getEncoder.encodeToString(s"$client_id:$client_secret".getBytes)
    val post = requests.post(
      "https://accounts.spotify.com/api/token",
      data = Map("grant_type" -> "client_credentials"),
      headers = Map("Authorization" -> s"Basic $auth")
    )
    (Json.parse(post.text) \ "access_token").as[String]
  }

  private def get(url: String): String = {
    val request = requests.get(url, headers = Map("Authorization" -> s"Bearer $token"))
    request.text
  }

  private implicit val userReads: Reads[User] = Json.reads[User]
  case class User(id: String) {
    private def getPlaylists(url: String = s"https://api.spotify.com/v1/users/$id/playlists"): Seq[Playlist] = {
      val jsonObj = Json.parse(get(url))
      val playlists = (jsonObj \ "items").as[Seq[Playlist]]
      (jsonObj \ "next").asOpt[String] match {
        case Some(next) => playlists ++ getPlaylists(next)
        case None => playlists
      }
    }

    private lazy val jsonObj: JsValue = Json.parse(get(s"https://api.spotify.com/v1/users/$id"))

    lazy val name: String = (jsonObj \ "display_name").as[String]
    lazy val playlists: Seq[Playlist] = getPlaylists()
  }

  private implicit val artistReads: Reads[Artist] = Json.reads[Artist]
  case class Artist(id: String, name: String)

  private implicit val playlistReads: Reads[Playlist] = Json.reads[Playlist]
  case class Playlist(id: String, name: String, description: String) {
    private def getTracks(url: String = s"https://api.spotify.com/v1/playlists/$id/tracks?fields=items(added_by.id%2Cadded_at%2Ctrack(id%2Cname%2Calbum(name%2Cartists(id%2Cname))))%2Cnext"): Seq[PlaylistItem] = {
      val jsonObj = Json.parse(get(url))
      val tracks = (jsonObj \ "items").as[Seq[PlaylistItem]]
      (jsonObj \ "next").asOpt[String] match {
        case Some(next) => tracks ++ getTracks(next)
        case None => tracks
      }
    }
    lazy val tracks: Seq[PlaylistItem] = getTracks()
  }

  private implicit val trackReads: Reads[Track] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "album" \ "name").read[String] and
      (JsPath \ "album" \ "artists").read[Seq[Artist]]
    )(Track.apply _)
  case class Track(id: String, name: String, album: String, artists: Seq[Artist])

  private implicit val playlistItemReads: Reads[PlaylistItem] = Json.reads[PlaylistItem]
  case class PlaylistItem(track: Track, added_by: User, added_at: Date)
}

object Spotify {
  def main(args: Array[String]): Unit = {


    val api = Spotify(CLIENT_ID, CLIENT_SECRET)
    val user = api.User("noahrosamilia")
    println(user.name)
    println(user.playlists.head.tracks(2).track.name)
    println(user.playlists.head.tracks(2).added_at)
  }
}
