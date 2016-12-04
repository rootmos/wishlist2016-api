import scalaz.concurrent.Task
import scalaz.syntax.all._
import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Success, Failure}

trait ScalaFuturesToTaskConversion {
  implicit class ScalaFuture2Task[T](fut: Future[T])(implicit ec: ExecutionContext) {
    def asTask: Task[T] = Task.async {
      register =>
        fut.onComplete {
          case Success(v) => register(v.right)
          case Failure(ex) => register(ex.left)
        }
    }
  }
}
