trait DataGenerators extends Salt {
  def newUserId = User.Id(s"user-id-$salt")
  def newUser = User(newUserId)
  def newClientSecret = User.ClientSecret(s"client-id-$salt", s"secret-$salt")
}
