package net.ivoah.spotifinder

import javax.swing.border.EmptyBorder
import scala.swing.*

object ScrollPane {
  val emptyBorder = new EmptyBorder(0, 0, 0, 0)
  def apply(component: Component): ScrollPane = new ScrollPane(component) {
    border = emptyBorder
  }
} 
