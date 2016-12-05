import java.time.ZonedDateTime

import scalaz._, syntax.either._
import scalaz.concurrent.Task
import org.scalatest._
import com.github.jostly.scalatest.mock.MockitoSweetener
import com.github.jostly.scalatest.mock.mockito.Capturing
import io.circe._
import org.http4s.circe._
import io.circe.optics.JsonPath._
import org.http4s.{Request, Method, AuthedRequest, Status, Uri}
import pdi.jwt.{JwtCirce, JwtAlgorithm}

import scala.util.Random

class UserServiceSpec extends WordSpec with Matchers with MockitoSweetener with DataGenerators with Inside with Capturing {
  "UserService" should {
    "answer with user info" in new Fixture {
      val ui = newUserInfo(user.id)
      val events = PutUserInfo(ui) :: Nil

      val request = Request(Method.GET)
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      root.id.string.getOption(json) shouldBe Some(user.id.repr)
      root.name.string.getOption(json) shouldBe Some(ui.name)
    }

    "fetch user info using the external service" in new Fixture {
      val events = Nil

      val ui = newUserInfo(user.id)
      externalUserInfoFetcher.apply(user) returns Task.point(ui)

      val request = Request(Method.GET)
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      root.id.string.getOption(json) shouldBe Some(user.id.repr)
      root.name.string.getOption(json) shouldBe Some(ui.name)

      capturing {
        there was one(eventStore).insertEvent(verified[Event] by {
          case PutUserInfo(ui, _) =>
        })
      }
    }

    "fetch other users info given correct token" in new Fixture {
      val ui = newUserInfo(user.id)
      val events = PutUserInfo(ui) :: Nil

      val token = JwtCirce.encode(s"""{"sub":"${user.id.repr}"}""", friendSecret, JwtAlgorithm.HS256)
      val request = Request(Method.GET, Uri().withQueryParam("friend", token))

      val \/-(response) = service(AuthedRequest(newUser, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      root.id.string.getOption(json) shouldBe Some(user.id.repr)
      root.name.string.getOption(json) shouldBe Some(ui.name)

      verifyZeroInteractions(externalUserInfoFetcher)
    }

    "fetch other users info given correct token (case with no info)" in new Fixture {
      val events = Nil

      val token = JwtCirce.encode(s"""{"sub":"${user.id.repr}"}""", friendSecret, JwtAlgorithm.HS256)
      val request = Request(Method.GET, Uri().withQueryParam("friend", token))

      val \/-(response) = service(AuthedRequest(newUser, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.NoContent

      verifyZeroInteractions(externalUserInfoFetcher)
    }

    "reject fetching other users info when token is invalid" in new Fixture {
      val events = Nil

      val wrongSecret = Base64EncodedSecret("wrong-secret")
      val token = JwtCirce.encode(s"""{"sub":"${user.id.repr}"}""", wrongSecret, JwtAlgorithm.HS256)
      val request = Request(Method.GET, Uri().withQueryParam("friend", token))

      val \/-(response) = service(AuthedRequest(newUser, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Forbidden

      verifyZeroInteractions(externalUserInfoFetcher)
      verifyZeroInteractions(eventStore)
    }

    "reject fetching other users info when token is incomplete" in new Fixture {
      val events = Nil

      val token = JwtCirce.encode(s"""{}""", friendSecret, JwtAlgorithm.HS256)
      val request = Request(Method.GET, Uri().withQueryParam("friend", token))

      val \/-(response) = service(AuthedRequest(newUser, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Forbidden

      verifyZeroInteractions(externalUserInfoFetcher)
      verifyZeroInteractions(eventStore)
    }

    "answer with list of friend tokens" in new Fixture {
      val followedUser = newUser
      val followId = newFollowId
      val events = FollowEvent(user.id, followId, followedUser.id) :: Nil

      val request = Request(Method.GET, Uri(path = "/follows"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      inside(root.arr.getOption(json)) {
        case Some(f :: Nil) =>
          root.id.string.getOption(f) shouldBe Some(followId.repr)
          val Some(token) = root.token.string.getOption(f)
          FriendToken.validate(token, friendSecret) shouldBe followedUser.id.right
      }
    }

    "answer with empty list of follows" in new Fixture {
      val events = Nil

      val request = Request(Method.GET, Uri(path = "/follows"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      root.arr.getOption(json) should matchPattern { case Some(Nil) => }
    }

    "add follow given friend token" in new Fixture {
      val events = Nil

      val followedUserId = newUserId
      val followId = newFollowId
      val request = Request(Method.PUT, Uri(path = s"/follows/$followId"))
        .withBody(s"""{"token":"${FriendToken.issue(followedUserId, friendSecret)}"}""")
        .unsafePerformSync
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Accepted

      capturing {
        there was one(eventStore).insertEvent(verified[Event] by {
          case FollowEvent(user.id, followId, followedUserId, _) =>
        })
      }
    }

    "reject adding follow given invalid friend token" in new Fixture {
      val events = Nil

      val followedUserId = newUserId
      val followId = newFollowId
      val wrongSecret = Base64EncodedSecret("wrong-secret")
      val request = Request(Method.PUT, Uri(path = s"/follows/$followId"))
        .withBody(s"""{"token":"${FriendToken.issue(followedUserId, wrongSecret)}"}""")
        .unsafePerformSync
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Forbidden

      verifyZeroInteractions(eventStore)
    }

    "delete follow" in new Fixture {
      val events = Nil

      val followId = newFollowId
      val request = Request(Method.DELETE, Uri(path = s"/follows/$followId"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Accepted

      capturing {
        there was one(eventStore).insertEvent(verified[Event] by {
          case UnfollowEvent(user.id, followId, _) =>
        })
      }
    }

    "answer with empty list when follow was deleted" in new Fixture {
      val followedUser = newUser
      val followId = newFollowId
      val t1 = ZonedDateTime.now
      val t2 = t1.plusHours(1)
      val events = Random.shuffle(FollowEvent(user.id, followId, followedUser.id, t1) :: UnfollowEvent(user.id, followId, t2) :: Nil)

      val request = Request(Method.GET, Uri(path = "/follows"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok

      val json = response.as[Json].unsafePerformSync
      root.arr.getOption(json) should matchPattern { case Some(Nil) => }
    }
  }

  trait Fixture {
    val user = newUser
    def events: List[Event]
    val eventStore = mock[EventStore]
    val externalUserInfoFetcher = mock[User => Task[UserInfo]]
    eventStore.fetchEvents(user.id) returns Task.point(events)
    eventStore.insertEvent(any[Event]) returns Task.point(())
    val friendSecret = Base64EncodedSecret(s"secret-$salt")

    val service = UserService(eventStore, externalUserInfoFetcher, friendSecret)
  }

}
