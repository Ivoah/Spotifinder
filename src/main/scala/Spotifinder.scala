import java.awt.Image
import java.net.URL
import javax.swing.ImageIcon
import javax.swing.border.EmptyBorder
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.swing._
import scala.swing.event._

object Spotifinder extends MainFrame with App {
  def _with[A](source: Source, fn: Source => A): A = {
    val result = fn(source)
    source.close()
    result
  }

  val Array(client_id, client_secret) = _with(Source.fromResource("credentials.txt"), _.getLines().next().split(':'))
  val api = Spotify(client_id, client_secret)

  val users = _with(
    Source.fromResource("users.txt"),
    _.getLines().filter(!_.startsWith("#")).map(api.User).toSeq
  )

  val privatePlaylists = new Object {
    override def toString: String = "Private playlists"
    val playlists: Seq[api.Playlist] = _with(
      Source.fromResource("privatePlaylists.txt"),
      _.getLines().filter(!_.startsWith("#")).map(api.Playlist.fromId).toSeq
    )
  }

  val usersList = new ListView(privatePlaylists +: users) {
    selection.intervalMode = ListView.IntervalMode.Single
    selection.reactions += {
      case ListSelectionChanged(source, range, live) if !live && selection.items.nonEmpty =>
        playlistsList.listData = selection.items.head.playlists
        songsList.listData = Seq()
        infoPanel.track = None
    }
  }
  val playlistsList = new ListView[api.Playlist]() {
    selection.intervalMode = ListView.IntervalMode.Single
    selection.reactions += {
      case ListSelectionChanged(source, range, live) if !live && selection.items.nonEmpty =>
        songsList.listData = selection.items.head.tracks
        infoPanel.track = None
    }
  }
  val songsList = new ListView[api.PlaylistItem]() {
    selection.intervalMode = ListView.IntervalMode.Single
    selection.reactions += {
      case ListSelectionChanged(source, range, live) if !live && selection.items.nonEmpty =>
        infoPanel.track = selection.items.head
    }
  }
  val infoPanel = new BoxPanel(Orientation.Vertical) {
    private var _track: Option[api.PlaylistItem] = None
    def track: Option[api.PlaylistItem] = _track
    def track_=(new_track: Option[api.PlaylistItem]): Unit = {
      _track = new_track
      _track match {
        case Some(track) =>
          val icon = new ImageIcon(track.track.album.artwork)
          labels("artwork").icon = new ImageIcon(icon.getImage.getScaledInstance(250, 250, Image.SCALE_SMOOTH))
          labels("name").text = track.track.name
          labels("album").text = track.track.album.name
          labels("artists").text = track.track.artists.map(_.name).mkString(", ")
          labels("added_by").text = track.added_by.name
          labels("added_at").text = track.added_at.toString
        case None =>
          labels("artwork").icon = null
          labels.values.foreach(_.text = "")
      }
    }
    def track_=(new_track: api.PlaylistItem): Unit = {
      track = Some(new_track)
    }

    val labels: ListMap[String, Label] = collection.immutable.ListMap(
      "artwork" -> new Label {
        preferredSize = new Dimension(250, 250)
      },
      "name" -> new Label,
      "album" -> new Label,
      "artists" -> new Label,
      "added_by" -> new Label,
      "added_at" -> new Label
    )

    contents ++= labels.values
  }

  def makeSplitPanes(lists: Seq[Component]): SplitPane = {
    val emptyBorder = new EmptyBorder(0, 0, 0, 0)
    object SplitPane {
      def apply(left: Component, right: Component): SplitPane = new SplitPane(Orientation.Vertical, left, right) {
        border = emptyBorder
        continuousLayout = true
        resizeWeight = 0.5
      }
    }

    lists match {
      case second_last::last::Nil => SplitPane(
        new ScrollPane(second_last) {border = emptyBorder},
        new ScrollPane(last) {border = emptyBorder}
      )
      case head::tail => SplitPane(
        new ScrollPane(head) {border = emptyBorder},
        makeSplitPanes(tail)
      )
    }
  }

  val searchBar = new TextField
  val searchButton = new Button(Action("Search") {
    for (user <- usersList.listData; playlist <- user.playlists; track <- playlist.tracks) {
      println(s"$user, $playlist, $track")
    }
  })

  contents = new BorderPanel {
    layout(new BoxPanel(Orientation.Horizontal) {
      contents ++= Seq(searchBar, searchButton)
    }) = BorderPanel.Position.North
    layout(makeSplitPanes(Seq(usersList, playlistsList, songsList, infoPanel))) = BorderPanel.Position.Center
  }

  title = "Spotifinder"
  iconImage = new ImageIcon(getClass.getResource("icon.png")).getImage
  centerOnScreen()
  open()
}
