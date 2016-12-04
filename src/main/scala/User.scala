import scalaz._, Kleisli._, syntax.applicative._
import org.http4s.{Request, OAuth2BearerToken}
import org.http4s.headers.Authorization
import pdi.jwt.{JwtCirce, JwtAlgorithm, JwtBase64}
import javax.crypto.SecretKey
import io.circe._

import scala.util.{Success, Failure}

case class User(id: User.Id)

object User {
  case class Id(repr: String)

  trait Encoders {
    implicit val userIdEncoder = new Encoder[Id] {
      def apply(x: Id): Json = Json.fromString(x.repr)
    }
  }

  trait Decoders {
    implicit val userIdDecoder = new Decoder[Id] {
      def apply(c: HCursor) = c.as[String].right map Id
    }
  }

  case class Base64EncodedSecret(encoded: String) extends SecretKey {
    val decoded = JwtBase64.decode(encoded)
    def getAlgorithm(): String = ???
    def getEncoded(): Array[Byte] = decoded
    def getFormat(): String = "RAW"
  }

  case class ClientSecret(id: String, secret: Base64EncodedSecret)

  def authorize[F[_]](clientSecret: ClientSecret)(implicit M: MonadError[F, Failure]): Kleisli[F, Request, User] = kleisli { request =>
    request.headers.get(Authorization) match {
      case Some(h) =>
        h.credentials match {
          case t: OAuth2BearerToken =>
            JwtCirce.decodeJson(t.token, clientSecret.secret, JwtAlgorithm.allHmac) match {
              case Success(json) =>
                json.hcursor.downField("sub").as[String] match {
                  case Right(sub) => User(Id(sub)).point
                  case Left(f) => M.raiseError(Unauthorized("invalid token: missing sub"))
                }
              case Failure(t) =>
                M.raiseError(Unauthorized("invalid token", Some(t)))
            }
          case _ => M.raiseError(Unauthorized("wrong authorization method"))
        }
      case None => M.raiseError(Unauthorized("no authorization header"))
    }
  }


  sealed trait Failure
  case class Unauthorized(reason: String, throwable: Option[Throwable] = None) extends Failure
}
