import scalaz._, Kleisli._, syntax.applicative._
import org.http4s.{Request, OAuth2BearerToken}
import org.http4s.headers.Authorization
import pdi.jwt.{JwtCirce, JwtAlgorithm}
import io.circe._
import io.circe.generic.semiauto._

import scala.util.{Success, Failure}

case class User(id: User.Id, authToken: User.AuthToken)

case class UserInfo(id: User.Id, name: String)

object User {
  case class Id(repr: String)
  case class AuthToken(repr: String)

  trait Encoders {
    implicit val userIdEncoder = new Encoder[Id] {
      def apply(x: Id): Json = Json.fromString(x.repr)
    }

    implicit val userInfoEncoder: Encoder[UserInfo] = deriveEncoder[UserInfo]
  }

  trait Decoders {
    implicit val userIdDecoder = new Decoder[Id] {
      def apply(c: HCursor) = c.as[String].right map Id
    }

    implicit val userInfoDecoder: Decoder[UserInfo] = deriveDecoder[UserInfo]
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
                  case Right(sub) => User(Id(sub), AuthToken(t.token)).point
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
