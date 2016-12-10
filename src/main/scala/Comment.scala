import io.circe._
import io.circe.generic.semiauto._
import java.util.UUID
import java.time.ZonedDateTime

case class Comment(cid: Comment.Id, from: User.Id, wid: Wish.Id, owner: User.Id, body: String, time: ZonedDateTime)

object Comment {
  case class Id(repr: String)
  
  def newId(): Id = Id(UUID.randomUUID().toString)

  trait Encoders extends User.Encoders with Wish.Encoders with ZonedDateTimeEncoders {
    implicit val commentIdEncoder = new Encoder[Id] {
      def apply(x: Id): Json = Json.fromString(x.repr)
    }


    implicit val commentEncoder: Encoder[Comment] = deriveEncoder[Comment]
  }

  trait Decoders extends User.Decoders with Wish.Decoders with ZonedDateTimeDecoders {
    implicit val commentIdDecoder = new Decoder[Id] {
      def apply(c: HCursor) = c.as[String].right map Id
    }

    implicit val commentDecoder: Decoder[Comment] = deriveDecoder[Comment]
  }
}
