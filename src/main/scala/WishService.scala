import scalaz._, syntax.bind._
import scalaz.concurrent.Task
import scalaz.concurrent.Task._
import org.http4s.{AuthedService, AuthedRequest}
import org.http4s.dsl._
import org.http4s.circe._
import io.circe._
import io.circe.syntax._
import io.circe.optics.JsonPath._

object WishService extends Wish.Encoders with EventStoreInstances {
  def apply(eventStore: EventStore, listSecret: Base64EncodedSecret): AuthedService[User] = AuthedService[User] {
    case AuthedRequest(u, GET -> Root :? FriendToken(token)) =>
      FriendToken.validate(token, listSecret) match {
        case \/-(uid) => fetchWishlist(eventStore, uid)
        case -\/(_) => Forbidden()
      }

    case AuthedRequest(u, GET -> Root) =>
      fetchWishlist(eventStore, u.id)

    case AuthedRequest(u, req @ PUT -> Root / wid) =>
      req.as[Json] flatMap { w =>
        root.title.string.getOption(w) match {
          case Some(title) =>
            val wish = Wish(Wish.Id(wid), u.id, title)
            (eventStore += PutWishEvent(wish)) >> Task { wish.asJson.noSpaces } >>= Ok[String]
          case None => BadRequest()
        }
      }

    case AuthedRequest(u, DELETE -> Root / wid) =>
      (eventStore += ForgetWishEvent(u.id, Wish.Id(wid))) >> Accepted()

    case AuthedRequest(u, GET -> Root / "wishlist-token") =>
      val token = FriendToken.issue(u.id, listSecret)
      Ok(s"""{"token":"$token"}""")
  }

  private def fetchWishlist(eventStore: EventStore, uid: User.Id) =
    eventStore.fold(uid, Map.empty[Wish.Id, Wish]) {
      case (acc, PutWishEvent(w, _)) => acc + (w.id -> w)
      case (acc, ForgetWishEvent(_, wid, _)) => acc - wid
    } map { _.values.asJson.noSpaces } >>= Ok[String]
}
