import scalaz._, syntax.bind._
import scalaz.concurrent.Task
import scalaz.concurrent.Task._
import org.http4s.{AuthedService, AuthedRequest, Uri}
import org.http4s.headers.Location
import org.http4s.dsl._
import org.http4s.circe._
import io.circe._
import io.circe.syntax._
import io.circe.optics.JsonPath._

object WishService extends Wish.Encoders with EventStoreInstances with Comment.Encoders {
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

    case AuthedRequest(u, GET -> Root / wid / "comments" :? FriendToken(token)) =>
      FriendToken.validate(token, listSecret) match {
        case \/-(uid) => fetchComments(eventStore, uid, Wish.Id(wid))
        case -\/(_) => Forbidden()
      }

    case AuthedRequest(u, GET -> Root / wid / "comments") =>
      fetchComments(eventStore, u.id, Wish.Id(wid))

    case AuthedRequest(u, req @ POST -> Root / wid / "comment") =>
      req.as[Json] flatMap { c =>
        (root.token.string.getOption(c), root.body.string.getOption(c)) match {
          case (Some(token), Some(body)) =>
            FriendToken.validate(token, listSecret) match {
              case \/-(ownerId) =>
                val cid = Comment.newId
                val e = CommentEvent(
                  fromId = u.id,
                  commentId = cid,
                  wishId = Wish.Id(wid),
                  wishOwnerId = ownerId,
                  body = body)
                (eventStore += e) >> Created() map { r =>
                  r.putHeaders(Location(Uri(path = s"/$wid/comment/${cid.repr}")))
                }
              case -\/(_) => Forbidden()
            }
          case (None, Some(body)) =>
            val cid = Comment.newId
            val e = CommentEvent(
              fromId = u.id,
              commentId = cid,
              wishId = Wish.Id(wid),
              wishOwnerId = u.id,
              body = body)
            (eventStore += e) >> Created() map { r =>
              r.putHeaders(Location(Uri(path = s"/$wid/comment/${cid.repr}")))
            }
          case _ => BadRequest()
        }
      }

    case AuthedRequest(u, GET -> Root / "wishlist-token") =>
      val token = FriendToken.issue(u.id, listSecret)
      Ok(s"""{"token":"$token"}""")
  }

  private def fetchWishlist(eventStore: EventStore, uid: User.Id) =
    eventStore.fold(uid, Map.empty[Wish.Id, Wish]) {
      case (acc, PutWishEvent(w, _)) => acc + (w.id -> w)
      case (acc, ForgetWishEvent(_, wid, _)) => acc - wid
    } map { _.values.asJson.noSpaces } >>= Ok[String]

  private def fetchComments(eventStore: EventStore, owner: User.Id, wid: Wish.Id) =
    eventStore.fold(owner, Map.empty[Comment.Id, Comment]) {
      case (acc, CommentEvent(from, cid, wid2, _, body, time)) if wid == wid2 =>
        acc + (cid -> Comment(cid, from, wid2, owner, body, time))
    } map { _.values.asJson.noSpaces } >>= Ok[String]
}
