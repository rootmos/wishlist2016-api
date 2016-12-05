import java.time.Instant

import pdi.jwt.{JwtCirce, JwtAlgorithm}
import org.http4s.dsl.QueryParamDecoderMatcher

import scalaz._, syntax.either._
import scala.util.{Success, Failure}

object FriendToken extends QueryParamDecoderMatcher[String]("friend") {
  def issue(uid: User.Id, friendSecret: Base64EncodedSecret): String =
    JwtCirce.encode(s"""{"sub":"${uid.repr}","iat":${Instant.now.getEpochSecond}}""", friendSecret, JwtAlgorithm.HS256)

  def validate(token: String, friendSecret: Base64EncodedSecret): Failure \/ User.Id =
    for {
      claim <- JwtCirce.decodeJson(token, friendSecret, JwtAlgorithm.allHmac) match {
        case Success(c) => c.right
        case Failure(t) => InvalidToken(t).left
      }
      uid <- claim.hcursor.downField("sub").as[String] match {
        case Right(sub) => sub.right
        case Left(_) => IncompleteClaim.left
      }
    } yield User.Id(uid)

  sealed trait Failure
  case class InvalidToken(throwable: Throwable) extends Failure
  case object IncompleteClaim extends Failure
}

