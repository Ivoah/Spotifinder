import java.awt.Image
import javax.swing.ImageIcon
import javax.swing.border.EmptyBorder
import scala.io.Source
import scala.swing._
import scala.swing.event._
import com.github.weisj.darklaf.LafManager
import com.github.weisj.darklaf.settings.ThemeSettings

import java.io.PrintWriter
import scala.util.{Random, Try}

object Spotifinder extends MainFrame with App {
  LafManager.installTheme(LafManager.getPreferredThemeStyle)
  LafManager.enabledPreferenceChangeReporting(true)
  LafManager.addThemePreferenceChangeListener(e => LafManager.installTheme(e.getPreferredThemeStyle))

  val playlistsFile = MyPaths.dataDir.resolve("privatePlaylists.txt").toFile
  val usersFile = MyPaths.dataDir.resolve("users.txt").toFile

  val emptyBorder = new EmptyBorder(0, 0, 0, 0)
  val gear = new ImageIcon(getClass.getResource("gear.gif"))

  var queue = 0
  def runInBackground(block: => Unit): Unit = {
    new Thread {
      override def run(): Unit = {
        val font = searchButton.font
        searchButton.icon = gear
        queue += 1
        block
        queue -= 1
        if (queue == 0) {
          searchButton.icon = null
          searchButton.font = font
        }
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

  val Seq(client_id, client_secret) = Util._with(Source.fromResource("credentials.txt"), _.getLines().toSeq)
  val api = Spotify(client_id, "user-modify-playback-state")

  case class SearchResult(user: api.User, playlist: api.Playlist, track: api.PlaylistItem, index: Int) {
    override def toString: String = s"$user > $playlist > $track"
  }

  object PrivatePlaylists {
    def apply(playlists: Seq[api.Playlist]) = new PrivatePlaylists(playlists)
  }
  class PrivatePlaylists(_playlists: Seq[api.Playlist]) extends api.User("") {
    override val name = "Private playlists"
    override lazy val playlists: Seq[api.Playlist] = _playlists
  }

  var privatePlaylists = PrivatePlaylists(Try(Util._with(
    Source.fromFile(playlistsFile),
    _.getLines().filter(!_.startsWith("#")).map(api.Playlist.fromURI).toSeq
  )).getOrElse(Seq()))

  var users = Try(Util._with(
    Source.fromFile(usersFile),
    _.getLines().filter(!_.startsWith("#")).map(api.User.fromURI).toSeq
  )).getOrElse(Seq())

  val usersList = new ListView(privatePlaylists +: users) {
    selection.intervalMode = ListView.IntervalMode.Single
    selection.reactions += {
      case ListSelectionChanged(source, range, live) if !live && selection.items.nonEmpty => runInBackground {
        playlistsList.listData = selection.items.head.playlists
        songsList.playlist = None
        infoPanel.track = None
      }
    }
  }

  val playlistsList = new ListView[api.Playlist]() {
    private val popupMenu = new PopupMenu {
      contents ++= Seq(
        new MenuItem(Action("Play") {
          api.play(selection.items.head, 0)
        }),
        new MenuItem(Action("Shuffle") {
          api.play(selection.items.head, Random.nextInt(selection.items.head.tracks.length))
        })
      )
    }

    selection.intervalMode = ListView.IntervalMode.Single
    selection.reactions += {
      case ListSelectionChanged(source, range, live) if !live && selection.items.nonEmpty => runInBackground {
        songsList.playlist = selection.items.head
        infoPanel.track = None
      }
    }

    mouse.clicks.reactions += {
      case MouseClicked(source, point, modifiers, clicks, triggersPopup) if modifiers == 256 =>
        selectIndices(this.peer.locationToIndex(point))
        popupMenu.show(source, point.x, point.y)
    }
    listenTo(mouse.clicks)
  }

  val songsList = new ListView[api.PlaylistItem]() {
    private var _playlist: Option[api.Playlist] = None
    def playlist: Option[api.Playlist] = _playlist
    def playlist_=(new_playlist: Option[api.Playlist]): Unit = {
      _playlist = new_playlist
      _playlist match {
        case Some(playlist) => listData = playlist.tracks
        case None => listData = Seq()
      }
    }
    def playlist_=(new_playlist: api.Playlist): Unit = {
      playlist = Some(new_playlist)
    }

    private val popupMenu = new PopupMenu {
      contents ++= Seq(
        new MenuItem(Action("Play") {
          api.play(playlist.get, selection.leadIndex)
        }),
        new MenuItem(Action("Add to queue") {
          api.queue(selection.items.head.track)
        })
      )
    }

    selection.intervalMode = ListView.IntervalMode.Single
    selection.reactions += {
      case ListSelectionChanged(source, range, live) if !live && selection.items.nonEmpty => runInBackground {
        infoPanel.track = selection.items.head
      }
    }

    mouse.clicks.reactions += {
      case MouseClicked(source, point, modifiers, clicks, triggersPopup) if modifiers == 256 =>
        selectIndices(this.peer.locationToIndex(point))
        popupMenu.show(source, point.x, point.y)
    }
    listenTo(mouse.clicks)
  }

  val infoPanel = new BoxPanel(Orientation.Vertical) {
    private val missing = new ImageIcon(new ImageIcon(getClass.getResource("missing.png")).getImage.getScaledInstance(250, 250, Image.SCALE_SMOOTH))

    private var _track: Option[api.PlaylistItem] = None
    def track: Option[api.PlaylistItem] = _track
    def track_=(new_track: Option[api.PlaylistItem]): Unit = {
      _track = new_track
      _track match {
        case Some(track) =>
          val icon = new ImageIcon(track.track.album.artwork)
          artwork.icon = new ImageIcon(icon.getImage.getScaledInstance(250, 250, Image.SCALE_SMOOTH))
          labels("Name").text = track.track.name
          labels("Album").text = track.track.album.name
          labels("Artists").text = track.track.artists.map(_.name).mkString(", ")
          labels("Added by").text = track.added_by.name
          labels("Added at").text = track.added_at.toString
        case None =>
          artwork.icon = missing
          labels.foreach {case (name, label) => label.text = name}
      }
    }
    def track_=(new_track: api.PlaylistItem): Unit = {
      track = Some(new_track)
    }

    private val artwork = new Label {icon = missing}
    private val labels: collection.immutable.SeqMap[String, Label] = Seq(
      "Name", "Album", "Artists", "Added by", "Added at"
    ).map(name => name -> new Label(name)).to(collection.immutable.SeqMap)

    contents += artwork
    contents ++= labels.values
  }

  val resultsList = new ListView[SearchResult] {
    private val popupMenu = new PopupMenu {
      contents ++= Seq(
        new MenuItem(Action("Play") {
          api.play(selection.items.head.playlist, selection.items.head.index)
        }),
        new MenuItem(Action("Add to queue") {
          api.queue(selection.items.head.track.track)
        })
      )
    }

    selection.intervalMode = ListView.IntervalMode.Single
    selection.reactions += {
      case ListSelectionChanged(source, range, live) if !live && selection.items.nonEmpty => runInBackground {
        infoPanel.track = selection.items.head.track
      }
    }

    mouse.clicks.reactions += {
      case MouseClicked(source, point, modifiers, clicks, triggersPopup) if modifiers == 256 =>
        selectIndices(this.peer.locationToIndex(point))
        popupMenu.show(source, point.x, point.y)
    }
    listenTo(mouse.clicks)
  }
  val searchAction = Action("Search")(runInBackground {
    val query = s"(?i:${searchBar.text})".r.unanchored
    resultsList.listData = for (
      user <- usersList.listData;
      playlist <- user.playlists;
      (track, index) <- playlist.tracks.zipWithIndex
      if (query.matches(track.track.name)
        || track.track.artists.exists(artist => query.matches(artist.name))
        || query.matches(track.track.album.name))
    ) yield SearchResult(user, playlist, track, index)
    tabView.pages(1).title = s"Search results (${resultsList.listData.length})"
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

  menuBar = new MenuBar() {
    contents ++= Seq(
      new Menu("File") {
        contents ++= Seq(
          new MenuItem(Action("Edit private playlists...") {
            new EditDialog[api.Playlist](
              Spotifinder.this,
              "Edit private playlists",
              api.Playlist.fromURI,
              privatePlaylists.playlists.to(collection.mutable.Seq),
              data => {
                privatePlaylists = PrivatePlaylists(data)

                usersList.listData = privatePlaylists +: users
                playlistsList.listData = Seq()
                songsList.playlist = None
                infoPanel.track = None
              }
            ).open()
          }),
          new MenuItem(Action("Edit users...") {
            new EditDialog[api.User](
              Spotifinder.this,
              "Edit users",
              api.User.fromURI,
              users.to(collection.mutable.Seq),
              data => {
                users = data

                usersList.listData = privatePlaylists +: users
                playlistsList.listData = Seq()
                songsList.playlist = None
                infoPanel.track = None
              }
            ).open()
          }),
          new MenuItem(Action("Save lists...") {
            if (Dialog.showConfirmation(
              this,
              "Are you sure you want to overwrite your saved playlists and users?",
              "Save lists",
              messageType = Dialog.Message.Warning
            ) == Dialog.Result.Yes) {
              playlistsFile.getParentFile.mkdirs()
              val playlistsWriter = new PrintWriter(playlistsFile)
              privatePlaylists.playlists.foreach(playlist => playlistsWriter.println(playlist.uri))
              playlistsWriter.close()

              usersFile.getParentFile.mkdirs()
              val usersWriter = new PrintWriter(usersFile)
              users.foreach(user => usersWriter.println(user.uri))
              usersWriter.close()
            }
          })
        )
      },
      new Menu("Settings") {
        contents ++= Seq(
          new MenuItem(Action("Clear cache...") {
            if (Dialog.showConfirmation(
              this,
              "Are you sure you want to clear the cache?",
              "Clear cache",
              messageType = Dialog.Message.Warning
            ) == Dialog.Result.Yes) api.clearCache()
          }),
          new MenuItem(Action("Change theme...") {ThemeSettings.showSettingsDialog(this.peer)})
        )
      }
    )
  }

  title = "Spotifinder"
  iconImage = new ImageIcon(getClass.getResource("icon.png")).getImage
  centerOnScreen()
  open()
}
