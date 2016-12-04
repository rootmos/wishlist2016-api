import scalaz._, syntax.all._
import org.scalatest._
import org.http4s._
import pdi.jwt.JwtCirce
import pdi.jwt.JwtAlgorithm

class UserSpec extends WordSpec with Matchers with DataGenerators {
  "User" should {
    "have an authorize method that" should {
      "reject request without Authorization header" in {
        val request = Request()
        val result = User.authorize[User.Failure \/ ?](newClientSecret).run(request)
        result shouldBe User.Unauthorized("no authorization header").left
      }

      "accept request with correct Authorization header" in {
        val clientSecret = newClientSecret
        val userId = newUserId
        val token = JwtCirce.encode(s"""{"sub":"${userId.repr}"}""", clientSecret.secret, JwtAlgorithm.HS512)

        val request = Request(headers = Headers(Header("authorization", s"Bearer $token")))
        val result = User.authorize[User.Failure \/ ?](clientSecret).run(request)
        result shouldBe \/-(User(userId))
      }

      "reject request with wrong authorization method" in {
        val request = Request(headers = Headers(Header("authorization", s"Basic yo")))
        val result = User.authorize[User.Failure \/ ?](newClientSecret).run(request)
        result shouldBe User.Unauthorized("wrong authorization method").left
      }

      "reject request with incorrect secret" in {
        val clientSecret = newClientSecret
        val userId = newUserId
        val claim = s"""{"sub":"${userId.repr}"}"""
        val token = JwtCirce.encode(claim, clientSecret.secret, JwtAlgorithm.HS512)

        val request = Request(headers = Headers(Header("authorization", s"Bearer $token")))
        val wrongSecret = clientSecret.copy(secret = "wrong-secret")
        val result = User.authorize[User.Failure \/ ?](wrongSecret).run(request)
        result should matchPattern { case -\/(User.Unauthorized("invalid token", _)) => }
      }

      "reject request without sub" in {
        val clientSecret = newClientSecret
        val claim = s"""{}"""
        val token = JwtCirce.encode(claim, clientSecret.secret, JwtAlgorithm.HS512)

        val request = Request(headers = Headers(Header("authorization", s"Bearer $token")))
        val result = User.authorize[User.Failure \/ ?](clientSecret).run(request)
        result shouldBe User.Unauthorized("invalid token: missing sub").left
      }

      "reject request with malformed token" in {
        val request = Request(headers = Headers(Header("authorization", s"Bearer yo")))
        val result = User.authorize[User.Failure \/ ?](newClientSecret).run(request)
        result should matchPattern { case -\/(User.Unauthorized("invalid token", _)) => }
      }
    }
  }
}
