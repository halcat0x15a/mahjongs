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
}
