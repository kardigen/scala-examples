package example2

object Main extends App {

  val romanNumbers = Map( 'I' -> 1, 'V' -> 5, 'X' -> 10, 'M' -> 50)

  def toNumber(roman: String): Int = {
    val mapping = romanNumbers withDefaultValue 0
    (roman.toUpperCase.toList map (mapping(_))).sum
  }

 println( toNumber("MMXXVII") )
}
