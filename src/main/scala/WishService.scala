import java.time.Instant

import scalaz._, syntax.bind._
import scalaz.concurrent.Task
import scalaz.concurrent.Task._
import org.http4s.{AuthedService, AuthedRequest}
import org.http4s.dsl._
import org.http4s.circe._
import io.circe._
import io.circe.syntax._
import io.circe.optics.JsonPath._
import pdi.jwt.{JwtCirce, JwtAlgorithm}

import scala.util.{Success, Failure}

object WishService extends Wish.Encoders with EventStoreInstances {
  def apply(eventStore: EventStore, listSecret: Base64EncodedSecret): AuthedService[User] = AuthedService[User] {
    case AuthedRequest(u, GET -> Root / "wishlist" ) =>
      fetchWishlist(eventStore, u.id)

    case AuthedRequest(u, req @ PUT -> Root / "wish" / wid) =>
      req.as[Json] flatMap { w =>
        root.title.string.getOption(w) match {
          case Some(title) =>
            val wish = Wish(Wish.Id(wid), u.id, title)
            eventStore.insertEvent(PutWishEvent(wish)) >> Task { wish.asJson.noSpaces } >>= Ok[String]
          case None => BadRequest()
        }
      }

    case AuthedRequest(u, DELETE -> Root / "wish" / wid) =>
      eventStore.insertEvent(ForgetWishEvent(u.id, Wish.Id(wid))) >> Accepted()

    case AuthedRequest(u, GET -> Root / "wishlist" / token) =>
      JwtCirce.decodeJson(token, listSecret, JwtAlgorithm.allHmac) match {
        case Success(json) =>
          json.hcursor.downField("sub").as[String] match {
            case Right(sub) => fetchWishlist(eventStore, User.Id(sub))
            case Left(f) => Forbidden()
          }
        case Failure(f) => Forbidden()
      }

    case AuthedRequest(u, GET -> Root / "wishlist-token") =>
      val token = JwtCirce.encode(s"""{"sub":"${u.id.repr}","iat":${Instant.now.getEpochSecond}}""", listSecret, JwtAlgorithm.HS256)
      Ok(s"""{"token":"$token"}""")
  }

  private def fetchWishlist(eventStore: EventStore, uid: User.Id) =
    eventStore.fold(uid, Map.empty[Wish.Id, Wish]) {
      case (acc, PutWishEvent(w, _)) => acc + (w.id -> w)
      case (acc, ForgetWishEvent(_, wid, _)) => acc - wid
    } map { _.values.asJson.noSpaces } >>= Ok[String]
}
