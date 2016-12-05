import pdi.jwt.JwtBase64

trait DataGenerators extends Salt {
  def newUserId = User.Id(s"user-id-$salt")
  def newAuthToken = User.AuthToken(s"token-$salt")
  def newUser = User(newUserId, newAuthToken)
  def newClientSecret = User.ClientSecret(
    s"client-id-$salt",
    Base64EncodedSecret(JwtBase64.encodeString(s"secret-$salt"))
  )

  def newWishId = Wish.Id(s"wish-id-$salt")
  def newTitle = s"title-$salt"
  def newWish(uid: User.Id) = Wish(newWishId, uid, title = newTitle)

  def newName = s"title-$salt"
  def newUserInfo(uid: User.Id) = UserInfo(uid, name = newName)
}
