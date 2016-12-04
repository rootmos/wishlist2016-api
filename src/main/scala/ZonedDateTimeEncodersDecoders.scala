import io.circe._
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import scala.util.{Try, Success, Failure}

trait ZonedDateTimeEncoders {
  implicit val zdtEncoder = new Encoder[ZonedDateTime] {
    def apply(zdt: ZonedDateTime): Json = Json.fromString(zdt.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
  }
}

trait ZonedDateTimeDecoders {
  implicit val zdtDecoder = new Decoder[ZonedDateTime] {
    def apply(c: HCursor) = c.as[String].right flatMap { s =>
      Try { ZonedDateTime.parse(s) } match {
        case Success(x) => Right(x)
        case Failure(t) => Left(DecodingFailure.fromThrowable(t, c.history))
      }
    }
  }
}
