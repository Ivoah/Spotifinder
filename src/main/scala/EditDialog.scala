package net.ivoah.spotifinder

import javax.swing.table.AbstractTableModel
import scala.swing.{Dialog, Table, Window}
import scala.util.Try

class EditDialog[A <: SpotifyItem](owner: Window, _title: String, _factory: String => A, var data: collection.mutable.Seq[A], _update: Seq[A] => Unit) extends Dialog(owner) {
  title = _title
  modal = true
  contents = ScrollPane(new Table {
    selection.elementMode = Table.ElementMode.None
    model = new AbstractTableModel {
      def getRowCount: Int = data.length + 1
      def getColumnCount: Int = 2
      override def getColumnName(columnIndex: Int): String = Seq("name", "uri")(columnIndex)
      override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = columnIndex == 1
      def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {
        if (rowIndex == data.length) ""
        else Seq(data(rowIndex).name, data(rowIndex).uri)(columnIndex)
      }
      override def setValueAt(uriAny: Any, rowIndex: Int, columnIndex: Int): Unit = {
        val uri = uriAny.asInstanceOf[String]
        val itemOption =
          if (uri.isEmpty) None
          else Try(_factory(uri)).toOption

        itemOption match {
          case Some(item) if rowIndex < data.length =>
            data(rowIndex) = item
          case Some(item) =>
            data :+= item
          case None if rowIndex < data.length =>
            data = data.zipWithIndex.collect {case (u, i) if i != rowIndex => u}
          case _ =>
        }

        _update(data.toSeq)
      }
    }
  })
}
