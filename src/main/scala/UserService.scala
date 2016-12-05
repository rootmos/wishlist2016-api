import scalaz._, syntax.bind._
import scalaz.concurrent.Task
import org.http4s.{AuthedService, AuthedRequest}
import org.http4s.dsl._
import org.http4s.circe._
import io.circe._
import io.circe.syntax._
import io.circe.optics.JsonPath._

object UserService extends User.Encoders with Follow.Encoders with EventStoreInstances {
  def apply(eventStore: EventStore, externalUserInfoFetcher: User => Task[UserInfo], friendSecret: Base64EncodedSecret): AuthedService[User] = AuthedService {
    case AuthedRequest(u, GET -> Root :? FriendToken(token)) =>
      FriendToken.validate(token, friendSecret) match {
        case \/-(uid) => fetchUserInfo(eventStore, uid) >>= {
          case Some(ui) => Task { ui.asJson.noSpaces } >>= Ok[String]
          case None => NoContent()
        }
        case -\/(_) => Forbidden()
      }

    case AuthedRequest(u, GET -> Root) => fetchUserInfo(eventStore, u.id) >>= {
      case Some(ui) => Task { ui.asJson.noSpaces } >>= Ok[String]
      case None =>
        for {
          ui <- externalUserInfoFetcher(u)
          _ <- eventStore += PutUserInfo(ui)
          resp <- Task { ui.asJson.noSpaces } >>= Ok[String]
        } yield resp
    }

    case AuthedRequest(u, GET -> Root / "follows") =>
      eventStore.fold(u.id, Map.empty[Follow.Id, Follow]) {
        case (acc, FollowEvent(_, fid, fuid, _)) =>
          acc + (fid -> Follow(fid, token = FriendToken.issue(fuid, friendSecret)))
        case (acc, UnfollowEvent(_, fid, _)) =>
          acc - fid
      } map { _.values.asJson.noSpaces} >>= Ok[String]

    case AuthedRequest(u, req @ PUT -> Root / "follows" / fid) =>
      req.as[Json] flatMap { w =>
        root.token.string.getOption(w) match {
          case Some(token) => FriendToken.validate(token, friendSecret) match {
            case \/-(fuid) => (eventStore += FollowEvent(u.id, Follow.Id(fid), fuid)) >> Accepted()
            case _ => Forbidden()
          }
          case None => BadRequest()
        }
      }

    case AuthedRequest(u, DELETE -> Root / "follows" / fid) =>
      (eventStore += UnfollowEvent(u.id, Follow.Id(fid))) >> Accepted()
  }

  private def fetchUserInfo(eventStore: EventStore, uid: User.Id): Task[Option[UserInfo]] = {
    eventStore.fold(uid, Option.empty[UserInfo]) {
      case (acc, PutUserInfo(ui, _)) => Some(ui)
    }
  }
}
