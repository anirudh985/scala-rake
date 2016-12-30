/**
  * Created by aj on 12/30/16.
  */
object Driver {
  def main(args: Array[String]): Unit = {
    val text: String = scala.io.StdIn.readLine("prompt> ")
    RAKE.run(text)
  }
}
