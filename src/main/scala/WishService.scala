import java.time.ZonedDateTime
import scalaz._, syntax.bind._
import scalaz.concurrent.Task
import scalaz.concurrent.Task._
import org.http4s.{AuthedService, AuthedRequest}
import org.http4s.dsl._
import org.http4s.circe._
import io.circe._
import io.circe.syntax._
import io.circe.optics.JsonPath._
import scala.math.Ordering

object WishService extends Wish.Encoders {
  def apply(eventStore: EventStore): AuthedService[User] = AuthedService[User] {
    case AuthedRequest(u, GET -> Root / "wishlist" ) =>
      eventStore.fetchEvents(u.id) map { _.sortBy(_.time) } map { es =>
        es.foldLeft(Map.empty[Wish.Id, Wish]) {
          case (acc, PutWishEvent(w, _)) => acc + (w.id -> w)
          case (acc, ForgetWishEvent(_, wid, _)) => acc - wid
        }
      } map { _.values.asJson.noSpaces } >>= Ok[String]

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
  }

  implicit val `ZonedDateTime has an Ordering` = new Ordering[ZonedDateTime] {
    def compare(x: ZonedDateTime, y: ZonedDateTime): Int = x compareTo y
  }
}
