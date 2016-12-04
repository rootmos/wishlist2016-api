import scalaz._, concurrent.Task._, Kleisli._
import org.http4s._, org.http4s.dsl._
import scalaz.concurrent.Task
import org.http4s.AuthedRequest
import org.http4s.server.{Server, ServerApp, AuthMiddleware, HttpMiddleware}
import org.http4s.server.blaze._
import scala.util.{Random, Properties}
import com.typesafe.scalalogging.StrictLogging


object Api extends ServerApp with StrictLogging {
  case class Config(port: Int, host: String, clientSecret: User.ClientSecret)

  val config = Config(
    port = Properties.envOrElse("PORT", "7000").toInt,
    host = Properties.envOrElse("HOST", "0.0.0.0"),
    clientSecret = User.ClientSecret(
      id = Properties.envOrNone("APP_CLIENT_ID").get,
      secret = User.Base64EncodedSecret(Properties.envOrNone("APP_CLIENT_SECRET").get)
    )
  )

  val auth: Kleisli[Task, Request, User] = User.authorize[EitherT[Task, User.Failure, ?]](config.clientSecret) mapT { _.run.map { case \/-(u) => u } }
  val authMiddleware = AuthMiddleware(auth)

  val helloService = AuthedService[User] {
    case AuthedRequest(u, GET -> Root / "hello" / name) =>
      Ok(s"Hello, $name.")
  }

  val logging: HttpMiddleware = { service =>
    kleisli { (request: Request) =>
      val reference = Random.alphanumeric.take(5).mkString
      logger.info(s"REQ-$reference: $request")
      service(request) map { response => logger.info(s"RSP-$reference: $response"); response }
    }
  }

  val service = logging(authMiddleware(helloService))

  override def server(args: List[String]): Task[Server] = {
    BlazeBuilder
      .bindHttp(config.port, config.host)
      .mountService(service, "/api")
      .start
  }
}
