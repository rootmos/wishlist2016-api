import scalaz._, syntax.bind._
import scalaz.concurrent.Task
import org.http4s.{AuthedService, AuthedRequest}
import org.http4s.dsl._
import io.circe.syntax._
import pdi.jwt.{JwtCirce, JwtAlgorithm}

import scala.util.{Success, Failure}

object UserService extends User.Encoders with EventStoreInstances {
  def apply(eventStore: EventStore, externalUserInfoFetcher: User => Task[UserInfo], friendSecret: Base64EncodedSecret): AuthedService[User] = AuthedService {
    case AuthedRequest(u, GET -> Root :? FriedToken(token)) =>
      JwtCirce.decodeJson(token, friendSecret, JwtAlgorithm.allHmac) match {
        case Success(json) =>
          json.hcursor.downField("sub").as[String] match {
            case Right(sub) => fetchUserInfo(eventStore, User.Id(sub)) >>= {
              case Some(ui) => Task { ui.asJson.noSpaces } >>= Ok[String]
              case None => NoContent()
            }
            case Left(f) => Forbidden()
          }
        case Failure(f) => Forbidden()
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
  }

  private def fetchUserInfo(eventStore: EventStore, uid: User.Id): Task[Option[UserInfo]] = {
    eventStore.fold(uid, Option.empty[UserInfo]) {
      case (acc, PutUserInfo(ui, _)) => Some(ui)
    }
  }

  object FriedToken extends QueryParamDecoderMatcher[String]("friend")
}
