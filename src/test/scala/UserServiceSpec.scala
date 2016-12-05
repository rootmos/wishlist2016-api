import scalaz._
import scalaz.concurrent.Task
import org.scalatest._
import com.github.jostly.scalatest.mock.MockitoSweetener
import com.github.jostly.scalatest.mock.mockito.Capturing
import io.circe._
import org.http4s.circe._
import io.circe.optics.JsonPath._
import org.http4s.{Request, Method, AuthedRequest, Status, Uri}

class UserServiceSpec extends WordSpec with Matchers with MockitoSweetener with DataGenerators with Inside with Capturing {
  "UserService" should {
    "answer with user info" in new Fixture {
      val ui = newUserInfo(user.id)
      val events = PutUserInfo(ui) :: Nil

      val request = Request(Method.GET, Uri(path = "/user"))
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

      val request = Request(Method.GET, Uri(path = "/user"))
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

    "fetch other users info given correct token" in {
      pending
    }
  }

  trait Fixture {
    val user = newUser
    def events: List[Event]
    val eventStore = mock[EventStore]
    val externalUserInfoFetcher = mock[User => Task[UserInfo]]
    eventStore.fetchEvents(user.id) returns Task.point(events)
    eventStore.insertEvent(any[Event]) returns Task.point(())

    val service = UserService(eventStore, externalUserInfoFetcher)
  }
}
