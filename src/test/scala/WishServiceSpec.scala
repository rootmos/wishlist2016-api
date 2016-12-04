import java.time.ZonedDateTime
import scalaz._
import scalaz.concurrent.Task
import org.scalatest._
import com.github.jostly.scalatest.mock.MockitoSweetener
import com.github.jostly.scalatest.mock.mockito.Capturing
import org.http4s.{Request, Method, AuthedRequest, Status, Uri}
import org.http4s.circe._
import io.circe._
import io.circe.optics.JsonPath._
import scala.util.Random

class WishServiceSpec extends WordSpec with Matchers with MockitoSweetener with DataGenerators with Inside with Capturing {
  "WishService" should {
    "answer with wishes" in new Fixture {
      val wish = newWish(user.id)
      val events = PutWishEvent(wish) :: Nil

      val request = Request(Method.GET, Uri(path = "/wishlist"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.arr.getOption(json)) {
        case Some(w :: Nil) =>
          root.id.string.getOption(w) shouldBe Some(wish.id.repr)
          root.uid.string.getOption(w) shouldBe Some(user.id.repr)
          root.title.string.getOption(w) shouldBe Some(wish.title)
      }
    }

    "answer with updated wish" in new Fixture {
      val t1 = ZonedDateTime.now
      val origWish = newWish(user.id)

      val t2 = t1.plusHours(1)
      val updatedWish = origWish.copy(title = newTitle)
      val events = Random.shuffle(PutWishEvent(origWish, t1) :: PutWishEvent(updatedWish, t2) :: Nil)

      val request = Request(Method.GET, Uri(path = "/wishlist"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.arr.getOption(json)) {
        case Some(w :: Nil) =>
          root.title.string.getOption(w) shouldBe Some(updatedWish.title)
      }
    }

    "answer with empty list when wish was deleted" in new Fixture {
      val wish = newWish(user.id)
      val t1 = ZonedDateTime.now
      val t2 = t1.plusHours(1)
      val events = Random.shuffle(PutWishEvent(wish, t1) :: ForgetWishEvent(user.id, wish.id, t2) :: Nil)

      val request = Request(Method.GET, Uri(path = "/wishlist"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.arr.getOption(json)) {
        case Some(xs) => xs shouldBe empty
      }
    }

    "answer with empty list when no events exist" in new Fixture {
      val events = Nil

      val request = Request(Method.GET, Uri(path = "/wishlist"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.arr.getOption(json)) {
        case Some(xs) => xs shouldBe empty
      }
    }

    "put updated wishes " in new Fixture {
      val events = Nil
      val wid = newWishId

      val title = newTitle
      val request = Request(Method.PUT, Uri(path = s"/wish/${wid.repr}"))
        .withBody(s"""{"title":"$title"}""")
        .unsafePerformSync

      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val w = response.as[Json].unsafePerformSync
      root.id.string.getOption(w) shouldBe Some(wid.repr)
      root.uid.string.getOption(w) shouldBe Some(user.id.repr)
      root.title.string.getOption(w) shouldBe Some(title)

      capturing {
        there was one(eventStore).insertEvent(verified[Event] by {
          case PutWishEvent(Wish(wid, user.id, title), _) =>
        })
      }
    }

    "delete wishes " in new Fixture {
      val events = Nil
      val wid = newWishId

      val request = Request(Method.DELETE, Uri(path = s"/wish/${wid.repr}"))

      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Accepted

      capturing {
        there was one(eventStore).insertEvent(verified[Event] by {
          case ForgetWishEvent(user.id, wid, _) =>
        })
      }
    }
  }

  trait Fixture {
    val user = newUser
    def events: List[Event]
    val eventStore = mock[EventStore]
    eventStore.fetchEvents(user.id) returns Task.point(events)
    eventStore.insertEvent(any[Event]) returns Task.point(())

    val service = WishService(eventStore)
  }
}
