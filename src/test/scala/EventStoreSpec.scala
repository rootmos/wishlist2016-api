import scalaz._, syntax.all._
import org.scalatest._
import scala.util.Properties

class EventStoreSpec extends WordSpec with Matchers with DataGenerators {
  "EventStore" should {
    "return empty list for new user ids" in new Fixture {
      eventStore.fetchEvents(newUserId).unsafePerformSyncAttempt shouldBe Nil.right
    }

    "return a previously put event" in new Fixture {
      val e = PutWishEvent(newWish(newUserId))
      eventStore.insertEvent(e).unsafePerformSyncAttempt shouldBe ().right
      eventStore.fetchEvents(e.userId).unsafePerformSyncAttempt should matchPattern {
        case \/-(PutWishEvent(e.wish, t) :: Nil) if e.time.isEqual(t) =>
      }
    }

    "return two previously put events" in new Fixture {
      val e1 = PutWishEvent(newWish(newUserId))
      val uid = e1.userId
      val wid = e1.wish.id
      eventStore.insertEvent(e1).unsafePerformSyncAttempt shouldBe ().right

      val e2 = ForgetWishEvent(uid, wid)
      eventStore.insertEvent(e2).unsafePerformSyncAttempt shouldBe ().right

      eventStore.fetchEvents(uid).unsafePerformSyncAttempt should matchPattern {
        case \/-(PutWishEvent(e1.wish, _) :: ForgetWishEvent(uid, wid, _) :: Nil) =>
      }
    }
  }

  trait Fixture {
    val connectionUrl = Properties.envOrNone("JDBC_DATABASE_URL").get
    val eventStore = new EventStore(connectionUrl)
  }
}
