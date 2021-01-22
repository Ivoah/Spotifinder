import scala.io.Source

object Util {
  def _with[A](source: Source, fn: Source => A): A = {
    val result = fn(source)
    source.close()
    result
  }
}
