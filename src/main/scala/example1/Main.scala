package example1


object Main extends App {
  val a = (1 to 5).toSet

  println(a map (_ + 7))
  println(a.nonEmpty)
  println(a contains 5)
}