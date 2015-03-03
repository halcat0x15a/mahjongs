package object mahjongs {
  def sequence[A](xs: Seq[Seq[A]]): Seq[Seq[A]] = xs.foldLeft(Seq(Seq[A]()))((a, b) => for (xs <- a; x <- b) yield xs :+ x)
}
