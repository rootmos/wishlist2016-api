import java.sql.Timestamp
import scalaz.concurrent.Task
import slick.driver.PostgresDriver
import scala.concurrent.ExecutionContext.Implicits.global
import io.circe.syntax._
import io.circe.parser._
import scalaz._, syntax.traverse._, std.list._, std.either._

class EventStore(val connectionUrl: String) extends EventStoreDB with ScalaFuturesToTaskConversion with Events.Encoders with Events.Decoders {
  import api._

  def insertEvent(e: Event): Task[Unit] = Task {
    val timestamp = Timestamp.from(e.time.toInstant)
    (e.userId.repr, timestamp, e.asJson.noSpaces)
  } flatMap { row =>
    db.run(events += row).asTask
  } map { _ => () }

  def fetchEvents(userId: User.Id): Task[List[Event]] =
    db.run(events.filter(_.user_id === userId.repr).map(_.payload).result).asTask map { _.toList } map {
      _ traverseU decode[Event] } map {
        case Right(xs) => xs
        case Left(f) => throw new RuntimeException(s"Unable to load events from database for user $userId: $f")
      }
}

trait EventStoreDB {

  def connectionUrl: String

  val api = PostgresDriver.api
  import api._

  class EventsTable(tag: Tag) extends Table[(String, Timestamp, String)](tag, "events") {
    def user_id = column[String]("user_id")
    def time = column[Timestamp]("time")
    def payload = column[String]("payload")
    def * = (user_id, time, payload)
  }
  val events = TableQuery[EventsTable]

  val db = Database.forURL(connectionUrl, driver = "org.postgresql.Driver")
}


