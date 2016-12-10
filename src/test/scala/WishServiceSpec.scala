import java.time.ZonedDateTime
import scalaz._
import scalaz.concurrent.Task
import org.scalatest._
import com.github.jostly.scalatest.mock.MockitoSweetener
import com.github.jostly.scalatest.mock.mockito.Capturing
import org.http4s.{Request, Method, AuthedRequest, Status, Uri}
import org.http4s.headers.Location
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
          case PutWishEvent(Wish(`wid`, user.id, `title`), _) =>
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
          case ForgetWishEvent(user.id, `wid`, _) =>
        })
      }
    }

    "fetch wishes given correct token" in new Fixture {
      val wish = newWish(user.id)
      val events = PutWishEvent(wish) :: Nil

      val token = JwtCirce.encode(s"""{"sub":"${user.id.repr}"}""", listSecret, JwtAlgorithm.HS256)
      val request = Request(Method.GET, Uri().withQueryParam("friend", token))

      val \/-(response) = service(AuthedRequest(newUser, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.arr.getOption(json)) {
        case Some(w :: Nil) =>
          root.id.string.getOption(w) shouldBe Some(wish.id.repr)
          root.uid.string.getOption(w) shouldBe Some(user.id.repr)
          root.title.string.getOption(w) shouldBe Some(wish.title)
      }
    }

    "reject fetching other's wishes with invalid token" in new Fixture {
      val events = Nil

      val wrongSecret = Base64EncodedSecret("wrong-secret")
      val token = JwtCirce.encode(s"""{"sub":"${user.id.repr}"}""", wrongSecret, JwtAlgorithm.HS256)
      val request = Request(Method.GET, Uri().withQueryParam("friend", token))

      val \/-(response) = service(AuthedRequest(newUser, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Forbidden

      verifyZeroInteractions(eventStore)
    }

    "reject fetching other's wishes with incomplete token" in new Fixture {
      val events = Nil

      val token = JwtCirce.encode(s"""{}""", listSecret, JwtAlgorithm.HS256)
      val request = Request(Method.GET, Uri().withQueryParam("friend", token))

      val \/-(response) = service(AuthedRequest(newUser, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Forbidden

      verifyZeroInteractions(eventStore)
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

    "add comment to own wish" in new Fixture {
      val events = Nil

      val wid = newWishId
      val body = s"body-$salt"
      val request = Request(Method.POST, Uri(path = s"/${wid.repr}/comment"))
        .withBody(s"""{"body":"$body"}""")
        .unsafePerformSync

      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Created

      val Some(location) = response.headers.get(Location) map { _.value }

      val urlMatcher = """/([^/]+)/comment/([^/]+)""".r
      val cid = inside(urlMatcher findFirstIn location) {
        case Some(urlMatcher(w, c)) =>
          w shouldBe wid.repr
          Comment.Id(c)
      }

      capturing {
        there was one(eventStore).insertEvent(verified[Event] by {
          case CommentEvent(user.id, `cid`, `wid`, user.id, `body`, _) =>
        })
      }
    }

    "add comment to wish owned by another user" in new Fixture {
      val events = Nil

      val wid = newWishId
      val owner = newUserId
      val token = FriendToken.issue(owner, listSecret)
      val body = s"body-$salt"
      val request = Request(Method.POST, Uri(path = s"/${wid.repr}/comment"))
        .withBody(s"""{"body":"$body", "token":"$token"}""")
        .unsafePerformSync

      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Created

      val Some(location) = response.headers.get(Location) map { _.value }

      val urlMatcher = """/([^/]+)/comment/([^/]+)""".r
      val cid = inside(urlMatcher findFirstIn location) {
        case Some(urlMatcher(w, c)) =>
          w shouldBe wid.repr
          Comment.Id(c)
      }

      capturing {
        there was one(eventStore).insertEvent(verified[Event] by {
          case CommentEvent(user.id, `cid`, `wid`, `owner`, `body`, _) =>
        })
      }
    }

    "fetch comments" in new Fixture {
      val wid = newWishId
      val from = newUserId
      val body = s"body-$salt"
      val cid = Comment.newId
      val e1 = CommentEvent(from, cid, wid, user.id, body)
      val e2 = CommentEvent(newUserId, Comment.newId, newWishId, user.id, s"other-body-$salt")
      val events = Random.shuffle(e1 :: e2 :: Nil)

      val request = Request(Method.GET, Uri(path = s"/${wid.repr}/comments"))

      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.arr.getOption(json)) {
        case Some(c :: Nil) =>
          root.cid.string.getOption(c) shouldBe Some(cid.repr)
          root.from.string.getOption(c) shouldBe Some(from.repr)
          root.wid.string.getOption(c) shouldBe Some(wid.repr)
          root.owner.string.getOption(c) shouldBe Some(user.id.repr)
          root.body.string.getOption(c) shouldBe Some(body)
          root.time.string.getOption(c) shouldBe a [Some[_]]
      }
    }

    "fetch comments using a friend token" in new Fixture {
      val wid = newWishId
      val from = newUserId
      val body = s"body-$salt"
      val cid = Comment.newId
      val e1 = CommentEvent(from, cid, wid, user.id, body)
      val events = e1 :: Nil

      val token = FriendToken.issue(user.id, listSecret)
      val request = Request(Method.GET, Uri(path = s"/${wid.repr}/comments").withQueryParam("friend", token))

      val \/-(response) = service(AuthedRequest(newUser, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.arr.getOption(json)) {
        case Some(c :: Nil) =>
          root.cid.string.getOption(c) shouldBe Some(cid.repr)
          root.from.string.getOption(c) shouldBe Some(from.repr)
          root.wid.string.getOption(c) shouldBe Some(wid.repr)
          root.owner.string.getOption(c) shouldBe Some(user.id.repr)
          root.body.string.getOption(c) shouldBe Some(body)
          root.time.string.getOption(c) shouldBe a [Some[_]]
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
