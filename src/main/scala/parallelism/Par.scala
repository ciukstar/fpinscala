package parallelism

trait Par[A]

object Par {
  def unit[A](a: => A): Par[A]
  def get(a: Par[A]): A
}
