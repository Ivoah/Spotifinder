import java.awt.Image
import javax.swing.ImageIcon
import javax.swing.border.EmptyBorder
import scala.io.Source
import scala.swing._
import scala.swing.event._

import com.github.weisj.darklaf.LafManager

object Spotifinder extends MainFrame with App {
  LafManager.installTheme(LafManager.getPreferredThemeStyle)
  LafManager.enabledPreferenceChangeReporting(true)
  LafManager.addThemePreferenceChangeListener(e => LafManager.installTheme(e.getPreferredThemeStyle))

  def _with[A](source: Source, fn: Source => A): A = {
    val result = fn(source)
    source.close()
    result
  }

  val emptyBorder = new EmptyBorder(0, 0, 0, 0)
  val gear = new ImageIcon(getClass.getResource("gear.gif"))

  def runInBackground(block: => Unit): Unit = {
    new Thread {
      override def run(): Unit = {
        val font = searchButton.font
        searchButton.icon = gear
        block
        searchButton.icon = null
        searchButton.font = font
      }
    }.start()
  }

  object ScrollPane {
    def apply(component: Component): ScrollPane = new ScrollPane(component) {
      border = emptyBorder
    }
  }

  def makeSplitPanes(lists: Seq[Component]): SplitPane = {
    object SplitPane {
      def apply(left: Component, right: Component): SplitPane = new SplitPane(Orientation.Vertical, left, right) {
        border = emptyBorder
        continuousLayout = true
        resizeWeight = 0.5
      }
    }

    lists match {
      case second_last::last::Nil => SplitPane(
        ScrollPane(second_last),
        ScrollPane(last)
      )
      case head::tail => SplitPane(
        ScrollPane(head),
        makeSplitPanes(tail)
      )
    }
  }

  val Seq(client_id, client_secret) = _with(Source.fromResource("credentials.txt"), _.getLines().toSeq)
  val api = Spotify(client_id, client_secret)

  case class SearchResult(user: api.User, playlist: api.Playlist, track: api.PlaylistItem) {
    override def toString: String = s"$user > $playlist > $track"
  }

  val users = _with(
    Source.fromResource("users.txt"),
    _.getLines().filter(!_.startsWith("#")).map(api.User).toSeq
  )

  val privatePlaylists = new api.User("") {
    override lazy val name = "Private playlists"
    override lazy val playlists: Seq[api.Playlist] = _with(
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
    private val missing = new ImageIcon(new ImageIcon(getClass.getResource("missing.png")).getImage.getScaledInstance(250, 250, Image.SCALE_SMOOTH))

    private var _track: Option[api.PlaylistItem] = None
    def track: Option[api.PlaylistItem] = _track
    def track_=(new_track: Option[api.PlaylistItem]): Unit = {
      _track = new_track
      _track match {
        case Some(track) => runInBackground {
          val icon = new ImageIcon(track.track.album.artwork)
          artwork.icon = new ImageIcon(icon.getImage.getScaledInstance(250, 250, Image.SCALE_SMOOTH))
          labels("Name").text = track.track.name
          labels("Album").text = track.track.album.name
          labels("Artists").text = track.track.artists.map(_.name).mkString(", ")
          labels("Added by").text = track.added_by.name
          labels("Added at").text = track.added_at.toString
        }
        case None =>
          artwork.icon = missing
          labels.foreach {case (name, label) => label.text = name}
      }
    }
    def track_=(new_track: api.PlaylistItem): Unit = {
      track = Some(new_track)
    }

    private val artwork = new Label {icon = missing}
    private val labels: collection.immutable.ListMap[String, Label] = Seq(
      "Name", "Album", "Artists", "Added by", "Added at"
    ).map(name => name -> new Label(name)).to(collection.immutable.ListMap)

    contents += artwork
    contents ++= labels.values
  }

  val resultsList = new ListView[SearchResult] {
    selection.intervalMode = ListView.IntervalMode.Single
    selection.reactions += {
      case ListSelectionChanged(source, range, live) if !live && selection.items.nonEmpty =>
        infoPanel.track = selection.items.head.track
    }
  }
  val searchAction = Action("Search")(runInBackground {
    val query = s"(?i:${searchBar.text})".r.unanchored
    resultsList.listData = for (
      user <- usersList.listData;
      playlist <- user.playlists;
      track <- playlist.tracks
      if (query.matches(track.track.name)
        || track.track.artists.exists(artist => query.matches(artist.name))
        || query.matches(track.track.album.name))
    ) yield SearchResult(user, playlist, track)
    tabView.selection.index = 1
  })
  val searchBar: TextField = new TextField {action = searchAction}
  val searchButton: Button = new Button(searchAction)

  val navView = makeSplitPanes(Seq(usersList, playlistsList, songsList))
  val searchView = ScrollPane(resultsList)
  val tabView = new TabbedPane {
    pages ++= Seq(
      new TabbedPane.Page("Playlists", navView),
      new TabbedPane.Page("Search results", searchView)
    )
  }

  contents = new BorderPanel {
    layout(new BoxPanel(Orientation.Horizontal) {
      contents ++= Seq(searchBar, searchButton)
    }) = BorderPanel.Position.North
    layout(new SplitPane(Orientation.Vertical, tabView, ScrollPane(infoPanel)) {
      border = emptyBorder
      continuousLayout = true
      resizeWeight = 1
    }) = BorderPanel.Position.Center
  }

  title = "Spotifinder"
  iconImage = new ImageIcon(getClass.getResource("icon.png")).getImage
  centerOnScreen()
  open()
}
