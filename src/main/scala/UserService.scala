import scalaz._, syntax.bind._
import scalaz.concurrent.Task
import org.http4s.{AuthedService, AuthedRequest}
import org.http4s.dsl._

object UserService extends User.Encoders with EventStoreInstances {
  def apply(eventStore: EventStore): AuthedService[User] = AuthedService {
    case AuthedRequest(u, GET -> Root / "user") => fetchUserInfo(eventStore, u.id) >>= {
      case Some(_) => Ok()
      case None => NoContent()
    }
  }

  private def fetchUserInfo(eventStore: EventStore, uid: User.Id): Task[Option[UserInfo]] = {
    eventStore.fold(uid, Option.empty[UserInfo]) {
      case (acc, PutUserInfo(ui, _)) => Some(ui)
    }
  }
}
