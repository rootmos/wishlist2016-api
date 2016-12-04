import io.circe._
import io.circe.generic.semiauto._
import java.time.ZonedDateTime

sealed trait Event {
  def userId: User.Id
  def time: ZonedDateTime
}

case class PutWishEvent(time: ZonedDateTime, wish: Wish) extends Event {
  def userId = wish.uid
}
case class ForgetWishEvent(userId: User.Id, time: ZonedDateTime, wishId: Wish.Id) extends Event

object Events {
  trait Encoders extends User.Encoders with Wish.Encoders with ZonedDateTimeEncoders {
    implicit val eventEncoder: Encoder[Event] = deriveEncoder[Event]
  }

  trait Decoders extends User.Decoders with Wish.Decoders with ZonedDateTimeDecoders {
    implicit val eventDecoder: Decoder[Event] = deriveDecoder[Event]
  }
}
