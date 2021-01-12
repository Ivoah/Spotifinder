import play.api.libs.functional.syntax.toFunctionalBuilderOps

import java.util.Date
import play.api.libs.json._

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

    override def toString: String = name
  }

  private implicit val artistReads: Reads[Artist] = Json.reads[Artist]
  case class Artist(id: String, name: String)

  private implicit val playlistReads: Reads[Playlist] = Json.reads[Playlist]
  object Playlist {
    def fromId(id: String): Playlist = {
      val jsonObj = Json.parse(get(s"https://api.spotify.com/v1/playlists/$id?fields=id%2Cname%2Cdescription"))
      jsonObj.as[Playlist]
    }
  }
  case class Playlist(id: String, name: String, description: String) {
    private def getTracks(url: String = s"https://api.spotify.com/v1/playlists/$id/tracks?items(added_by.id%2Cadded_at%2Ctrack(id%2Cname%2Calbum(id%2Cmages%2Cname)%2Cartists(id%2Cname)))%2Cnext"): Seq[PlaylistItem] = {
      val jsonObj = Json.parse(get(url))
      val tracks = (jsonObj \ "items").as[Seq[PlaylistItem]]
      (jsonObj \ "next").asOpt[String] match {
        case Some(next) => tracks ++ getTracks(next)
        case None => tracks
      }
    }
    lazy val tracks: Seq[PlaylistItem] = getTracks()

    override def toString: String = name
  }

  private implicit val albumReads: Reads[Album] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "images" \ 0 \ "url").read[String]
    )(Album.apply _)
  case class Album(id: String, name: String, artwork: String)

  private implicit val trackReads: Reads[Track] = Json.reads[Track]
  case class Track(id: String, name: String, album: Album, artists: Seq[Artist])

  private implicit val playlistItemReads: Reads[PlaylistItem] = Json.reads[PlaylistItem]
  case class PlaylistItem(track: Track, added_by: User, added_at: Date) {
    override def toString: String = track.name
  }
}

object Spotify {
  def main(args: Array[String]): Unit = {
    val CLIENT_ID = "f27ceb8f09e54d8286ad2fe0582aaad5"
    val CLIENT_SECRET = "24297f6dd14a457292dc5fd0111c416b"

    val api = Spotify(CLIENT_ID, CLIENT_SECRET)
    val user = api.User("noahrosamilia")
    println(user.name)
    println(user.playlists.head.tracks(2).track.name)
    println(user.playlists.head.tracks(2).added_at)
  }
}
