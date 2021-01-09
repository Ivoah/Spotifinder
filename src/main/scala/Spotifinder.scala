import javax.swing.border.EmptyBorder
import scala.swing._
import scala.swing.event._

object Spotifinder extends MainFrame with App {
  val emptyBorder = new EmptyBorder(0, 0, 0, 0)

  val ids = Seq("noahrosamilia", "1289823125")

  val lists = Seq(
    new ListView(Seq("Noah Rosamilia", "Emma Brower", "Allison Kay")) {
      println(listData)

      selection.intervalMode = ListView.IntervalMode.Single
      selection.reactions += {
        case ListSelectionChanged(source, range, live) if !live =>
          println(range)
          println(selection.items)
          println(selection.leadIndex, selection.anchorIndex)
      }
    },
    new ListView(Seq("Sunrise", "Driving", "I thought I hated country")),
    new ListView(Seq("Monsters of the North", "Better Place", "Si j'osais")),
    new Panel {}
  )

  def makeSplitPanes(lists: Seq[Component]): SplitPane = {
    lists match {
      case second_last::last::Nil => new SplitPane(Orientation.Vertical,
        new ScrollPane(second_last) {border = emptyBorder},
        new ScrollPane(last) {border = emptyBorder}
      ) {border = emptyBorder}
      case head::tail => new SplitPane(Orientation.Vertical,
        new ScrollPane(head) {border = emptyBorder},
        makeSplitPanes(tail)
      ) {border = emptyBorder}
    }
  }

  contents = makeSplitPanes(lists)

  pack()
  centerOnScreen()
  open()
}
