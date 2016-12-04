import scalaz._, syntax.all._
import org.scalatest._
import java.time.ZonedDateTime
import scala.util.Properties

class EventStoreSpec extends WordSpec with Matchers with DataGenerators {
  "EventStore" should {
    "return empty list for new user ids" in new Fixture {
      eventStore.fetchEvents(newUserId).unsafePerformSyncAttempt shouldBe Nil.right
    }

    "return a previously put event" in new Fixture {
      val e = PutWishEvent(ZonedDateTime.now, newWish(newUserId))
      eventStore.insertEvent(e).unsafePerformSyncAttempt shouldBe ().right
      eventStore.fetchEvents(e.userId).unsafePerformSyncAttempt should matchPattern {
        case \/-(PutWishEvent(t, e.wish) :: Nil) if e.time.isEqual(t) =>
      }
    }

    "return two previously put events" in new Fixture {
      val e1 = PutWishEvent(ZonedDateTime.now, newWish(newUserId))
      val uid = e1.userId
      val wid = e1.wish.id
      eventStore.insertEvent(e1).unsafePerformSyncAttempt shouldBe ().right

      val e2 = ForgetWishEvent(uid, ZonedDateTime.now, wid)
      eventStore.insertEvent(e2).unsafePerformSyncAttempt shouldBe ().right

      eventStore.fetchEvents(uid).unsafePerformSyncAttempt should matchPattern {
        case \/-(PutWishEvent(_, e1.wish) :: ForgetWishEvent(uid, _, wid) :: Nil) =>
      }
    }
  }

  trait Fixture {
    val connectionUrl = Properties.envOrNone("DATABASE_URL").get
    val eventStore = new EventStore(connectionUrl)
  }
}
