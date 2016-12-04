import javax.crypto.SecretKey
import pdi.jwt.JwtBase64

case class Base64EncodedSecret(encoded: String) extends SecretKey {
  val decoded = JwtBase64.decode(encoded)
  def getAlgorithm(): String = ???
  def getEncoded(): Array[Byte] = decoded
  def getFormat(): String = "RAW"
}


