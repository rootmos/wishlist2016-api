import pdi.jwt.JwtBase64

trait DataGenerators extends Salt {
  def newUserId = User.Id(s"user-id-$salt")
  def newUser = User(newUserId)
  def newClientSecret = User.ClientSecret(
    s"client-id-$salt",
    User.Base64EncodedSecret(JwtBase64.encodeString(s"secret-$salt"))
  )
}
