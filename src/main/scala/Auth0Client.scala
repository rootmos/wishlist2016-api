import scalaz.concurrent.Task
import org.http4s.{Request, Uri, Method}
import org.http4s.client.blaze._
import org.http4s.circe._
import io.circe.generic.auto._

class Auth0Client private (fetchUserInfoUri: Uri) {

  val fetchUserInfo: User => Task[UserInfo] = { user =>
    for {
      req <- Request(uri = fetchUserInfoUri, method = Method.POST)
        .withBody(TokenInfoRequest(user.authToken.repr))(jsonEncoderOf)
      resp <- httpClient.expect(req)(jsonOf[TokenInfoResponse])
    } yield UserInfo(user.id, resp.name)
  }

  private case class TokenInfoRequest(id_token: String)
  private case class TokenInfoResponse(name: String)

  private val httpClient = PooledHttp1Client()
}

object Auth0Client {
  def apply(clientDomain: String) =
    for {
      fetchUserInfoUri <- Uri.fromString(s"https://$clientDomain/tokeninfo")
    } yield new Auth0Client(fetchUserInfoUri)
}
