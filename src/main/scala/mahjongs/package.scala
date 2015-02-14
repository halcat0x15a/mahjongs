package object mahjongs {
  def sequence[A](xs: Seq[Seq[A]]): Seq[Seq[A]] = xs.foldLeft(Seq(Seq[A]()))((a, b) => for (xs <- a; x <- b) yield xs :+ x)
  trait Enum[A] {
    def values: Seq[A]
    implicit def ordering: Ordering[A] =
      new Ordering[A] {
        def compare(x: A, y: A) =
          values.indexOf(x) compareTo values.indexOf(y)
      }
  }
  implicit class BooleanOps(boolean: Boolean) {
    def option[A](value: A) = if (boolean) Some(value) else None
  }
  implicit class OptionIntOps(option: Option[Int]) {
    def +(that: Option[Int]) = (option, that) match {
      case (Some(n), Some(m)) => Some(n + m)
      case (Some(n), None) => Some(n)
      case (None, Some(m)) => Some(m)
      case (None, None) => None
    }
  }
}
