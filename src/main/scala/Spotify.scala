import play.api.libs.functional.syntax.toFunctionalBuilderOps

import java.util.Date
import play.api.libs.json._

import java.text.SimpleDateFormat

object Spotify {
  val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
  def get(url: String): String = {
    val request = requests.get(url, headers = Map("Authorization" -> "Bearer ???"))
    request.text
  }

  implicit val userReads: Reads[User] = Json.reads[User]
  case class User(id: String) {
    private def getPlaylists(url: String = s"https://api.spotify.com/v1/users/$id/playlists"): Seq[Playlist] = {
      val jsonObj = Json.parse(get(url))
      val playlists = (jsonObj \ "items").as[Seq[Playlist]]
      (jsonObj \ "next").asOpt[String] match {
        case Some(next) => playlists ++ getPlaylists(next)
        case None => playlists
      }
    }

    lazy val jsonObj: JsValue = Json.parse(get(s"https://api.spotify.com/v1/users/$id"))

    lazy val name: String = (jsonObj \ "display_name").as[String]
    lazy val playlists: Seq[Playlist] = getPlaylists()
  }

  implicit val artistReads: Reads[Artist] = Json.reads[Artist]
  case class Artist(id: String, name: String)

  implicit val playlistReads: Reads[Playlist] = Json.reads[Playlist]
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

  implicit val playlistItemReads: Reads[PlaylistItem] = (
    (JsPath \ "track" \ "id").read[String] and
      (JsPath \ "track" \ "name").read[String] and
      (JsPath \ "track" \ "album" \ "name").read[String] and
      (JsPath \ "track" \ "album" \ "artists").read[Seq[Artist]] and
      (JsPath \ "added_by").read[User] and
      (JsPath \ "added_at").read[String]
  ) { (id: String, name: String, album: String, artists: Seq[Artist], added_by: User, added_at: String) =>
    PlaylistItem(Track(id, name, album, artists), added_by, format.parse(added_at))
  }
  case class PlaylistItem(track: Track, added_by: User, added_at: Date)
  case class Track(id: String, name: String, album: String, artists: Seq[Artist])

  def main(args: Array[String]) = {
    val user = Spotify.User("noahrosamilia")
    println(user.name)
    println(user.playlists.head.tracks.head)
  }
}
