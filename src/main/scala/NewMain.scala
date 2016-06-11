import Implicit._

object NewMain {

  def main(args: Array[String]): Unit = {
    println()

    val s = new MutableSignal("")

    val x = s.filter(_ == "hi").count.forEach((a: Int) => println(a))

    x.printHierarchy()

    val input = List("potato", "hi", "ok", "ok", "hi")
    input.foreach(s.set)
  }
}