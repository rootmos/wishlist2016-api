import scalaz._, syntax.bind._
import org.http4s.{AuthedService, AuthedRequest}
import org.http4s.dsl._
import io.circe.syntax._

object WishService extends Wish.Encoders {
  def service(eventStore: EventStore): AuthedService[User] = AuthedService[User] {
    case AuthedRequest(u, GET -> Root / "wishlist" ) =>
      eventStore.fetchEvents(u.id) map { es =>
        es.foldRight(List.empty[Wish]) {
          case (PutWishEvent(w, _), acc) => acc :+ w
          case (_, acc) => acc
        }
      } map { _.asJson.noSpaces } >>= Ok[String]
  }
}
