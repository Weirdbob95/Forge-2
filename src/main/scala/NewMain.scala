import Implicit._

object NewMain {

  def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val s = new Signal("")

    s.filter(_ == "hi").count.forEach((a: Int) => println(a))

    val input = List("potato", "hi", "ok", "ok", "hi")
    input.foreach(s.set)
  }
}