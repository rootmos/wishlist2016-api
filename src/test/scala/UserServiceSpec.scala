import scalaz._
import scalaz.concurrent.Task
import org.scalatest._
import com.github.jostly.scalatest.mock.MockitoSweetener
import com.github.jostly.scalatest.mock.mockito.Capturing
import org.http4s.{Request, Method, AuthedRequest, Status, Uri}

class UserServiceSpec extends WordSpec with Matchers with MockitoSweetener with DataGenerators with Inside with Capturing {
  "UserService" should {
    "answer with user info" in new Fixture {
      val ui = newUserInfo(user.id)
      val events = PutUserInfo(ui) :: Nil

      val request = Request(Method.GET, Uri(path = "/user"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.Ok
    }

    "answer no content when no user info exist" in new Fixture {
      val events = Nil

      val request = Request(Method.GET, Uri(path = "/user"))
      val \/-(response) = service(AuthedRequest(user, request)).unsafePerformSyncAttempt
      response.status shouldBe Status.NoContent
    }

    "fetch other users info given correct token" in {
      pending
    }
  }

  trait Fixture {
    val user = newUser
    def events: List[Event]
    val eventStore = mock[EventStore]
    eventStore.fetchEvents(user.id) returns Task.point(events)
    eventStore.insertEvent(any[Event]) returns Task.point(())

    val service = UserService(eventStore)
  }
}
