import scala.util.Random

trait Salt {
  def salt: String = Random.alphanumeric.take(5).mkString
}

object Salt
