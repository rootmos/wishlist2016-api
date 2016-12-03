import org.http4s._, org.http4s.dsl._
import scalaz.concurrent.Task
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze._
import scala.util.Properties

object Api extends ServerApp {

  val service = HttpService {
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
  }

  override def server(args: List[String]): Task[Server] = {
    BlazeBuilder
      .bindHttp(Properties.envOrNone("PORT") map { _.toInt } get, "localhost")
      .mountService(service, "/api")
      .start
  }
}
