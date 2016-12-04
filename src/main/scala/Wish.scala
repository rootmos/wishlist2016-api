import io.circe._
import io.circe.generic.semiauto._

case class Wish(id: Wish.Id, uid: User.Id, title: String)

object Wish {
  case class Id(repr: String)

  trait Encoders extends User.Encoders {
    implicit val wishIdEncoder = new Encoder[Id] {
      def apply(x: Id): Json = Json.fromString(x.repr)
    }


    implicit val wishEncoder: Encoder[Wish] = deriveEncoder[Wish]
  }

  trait Decoders extends User.Decoders {
    implicit val wishIdDecoder = new Decoder[Id] {
      def apply(c: HCursor) = c.as[String].right map Id
    }

    implicit val wishDecoder: Decoder[Wish] = deriveDecoder[Wish]
  }
}
