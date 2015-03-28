package mahjongs

sealed trait Winning

case object Drawn extends Winning

case object Discard extends Winning
