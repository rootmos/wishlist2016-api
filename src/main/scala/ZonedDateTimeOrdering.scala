import java.time.ZonedDateTime
import scala.math.Ordering

trait ZonedDateTimeOrdering {
  implicit val `ZonedDateTime has an Ordering` = new Ordering[ZonedDateTime] {
    def compare(x: ZonedDateTime, y: ZonedDateTime): Int = x compareTo y
  }
}

object ZonedDateTimeOrdering extends ZonedDateTimeOrdering
