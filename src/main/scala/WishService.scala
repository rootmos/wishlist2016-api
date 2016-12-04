import java.time.ZonedDateTime
import scalaz._, syntax.bind._
import org.http4s.{AuthedService, AuthedRequest}
import org.http4s.dsl._
import io.circe.syntax._
import scala.math.Ordering

object WishService extends Wish.Encoders {
  def service(eventStore: EventStore): AuthedService[User] = AuthedService[User] {
    case AuthedRequest(u, GET -> Root / "wishlist" ) =>
      eventStore.fetchEvents(u.id) map { _.sortBy(_.time) } map { es =>
        es.foldLeft(Map.empty[Wish.Id, Wish]) {
          case (acc, PutWishEvent(w, _)) => acc + (w.id -> w)
          case (acc, ForgetWishEvent(_, wid, _)) => acc - wid
        }
      } map { _.values.asJson.noSpaces } >>= Ok[String]
  }

  implicit val `ZonedDateTime has an Ordering` = new Ordering[ZonedDateTime] {
    def compare(x: ZonedDateTime, y: ZonedDateTime): Int = x compareTo y
  }
}
