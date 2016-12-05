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
import pdi.jwt.{JwtCirce, JwtAlgorithm}

import scala.util.{Random, Success}

class WishServiceSpec extends WordSpec with Matchers with MockitoSweetener with DataGenerators with Inside with Capturing {
  "WishService" should {
    "answer with wishes" in new Fixture {
      val wish = newWish(user.id)
      val events = PutWishEvent(wish) :: Nil

      val request = Request(Method.GET)
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

      val request = Request(Method.GET)
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

      val request = Request(Method.GET)
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.arr.getOption(json)) {
        case Some(xs) => xs shouldBe empty
      }
    }

    "answer with empty list when no events exist" in new Fixture {
      val events = Nil

      val request = Request(Method.GET)
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
      val request = Request(Method.PUT, Uri(path = s"/${wid.repr}"))
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

      val request = Request(Method.DELETE, Uri(path = s"/${wid.repr}"))

      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Accepted

      capturing {
        there was one(eventStore).insertEvent(verified[Event] by {
          case ForgetWishEvent(user.id, wid, _) =>
        })
      }
    }

    "fetch wishes given correct token" in new Fixture {
      val wish = newWish(user.id)
      val events = PutWishEvent(wish) :: Nil

      val token = JwtCirce.encode(s"""{"sub":"${user.id.repr}"}""", listSecret, JwtAlgorithm.HS256)
      val request = Request(Method.GET, Uri().withQueryParam("friend", token))

      val otherUser = newUser
      eventStore.fetchEvents(otherUser.id) returns Task.point(Nil)
      val \/-(response) = service(AuthedRequest(otherUser, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.arr.getOption(json)) {
        case Some(w :: Nil) =>
          root.id.string.getOption(w) shouldBe Some(wish.id.repr)
          root.uid.string.getOption(w) shouldBe Some(user.id.repr)
          root.title.string.getOption(w) shouldBe Some(wish.title)
      }
    }

    "fetch wish-list token" in new Fixture {
      val events = Nil

      val request = Request(Method.GET, Uri(path = s"/wishlist-token"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.token.string.getOption(json)) {
        case Some(token) =>
          val Success(claim) = JwtCirce.decodeJson(token, listSecret, JwtAlgorithm.allHmac)
          root.sub.string.getOption(claim) shouldBe Some(user.id.repr)
          root.iat.int.getOption(claim) should matchPattern { case Some(_) => }

      }
    }
  }

  trait Fixture {
    val user = newUser
    def events: List[Event]
    val eventStore = mock[EventStore]
    eventStore.fetchEvents(user.id) returns Task.point(events)
    eventStore.insertEvent(any[Event]) returns Task.point(())
    val listSecret = Base64EncodedSecret(s"secret-$salt")

    val service = WishService(eventStore, listSecret)
  }
}
