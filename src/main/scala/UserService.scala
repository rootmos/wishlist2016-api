import scalaz._, syntax.bind._
import scalaz.concurrent.Task
import org.http4s.{AuthedService, AuthedRequest}
import org.http4s.dsl._
import io.circe.syntax._

object UserService extends User.Encoders with EventStoreInstances {
  def apply(eventStore: EventStore, externalUserInfoFetcher: User => Task[UserInfo]): AuthedService[User] = AuthedService {
    case AuthedRequest(u, GET -> Root / "user") => fetchUserInfo(eventStore, u.id) >>= {
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
}
