import io.circe._
import io.circe.generic.semiauto._

case class Follow(id: Follow.Id, token: String)

object Follow {
  case class Id(repr: String)

  trait Encoders extends User.Encoders {
    implicit val followIdEncoder = new Encoder[Id] {
      def apply(x: Id): Json = Json.fromString(x.repr)
    }

    implicit val followEncoder: Encoder[Follow] = deriveEncoder[Follow]
  }

  trait Decoders extends User.Decoders {
    implicit val followIdDecoder = new Decoder[Id] {
      def apply(c: HCursor) = c.as[String].right map Id
    }

    implicit val followDecoder: Decoder[Follow] = deriveDecoder[Follow]
  }
}
