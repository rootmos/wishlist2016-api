import io.circe._
import io.circe.generic.semiauto._
import java.time.ZonedDateTime

sealed trait Event {
  def userId: User.Id
  def time: ZonedDateTime
}

case class PutWishEvent(wish: Wish, time: ZonedDateTime = ZonedDateTime.now) extends Event {
  def userId = wish.uid
}
case class ForgetWishEvent(userId: User.Id, wishId: Wish.Id, time: ZonedDateTime = ZonedDateTime.now) extends Event

case class PutUserInfo(userInfo: UserInfo, time: ZonedDateTime = ZonedDateTime.now) extends Event {
  def userId = userInfo.id
}

case class FollowEvent(userId: User.Id, followId: Follow.Id, followsUser: User.Id, time: ZonedDateTime = ZonedDateTime.now) extends Event

case class UnfollowEvent(userId: User.Id, followId: Follow.Id, time: ZonedDateTime = ZonedDateTime.now) extends Event

case class CommentEvent(
  fromId: User.Id,
  commentId: Comment.Id,
  wishId: Wish.Id,
  wishOwnerId: User.Id,
  body: String,
  time: ZonedDateTime = ZonedDateTime.now) extends Event {
    def userId = wishOwnerId
  }

object Events {
  trait Encoders extends User.Encoders with Wish.Encoders with Follow.Encoders with ZonedDateTimeEncoders with Comment.Encoders {
    implicit val eventEncoder: Encoder[Event] = deriveEncoder[Event]
  }

  trait Decoders extends User.Decoders with Wish.Decoders with Follow.Decoders with ZonedDateTimeDecoders with Comment.Decoders {
    implicit val eventDecoder: Decoder[Event] = deriveDecoder[Event]
  }
}
