package mahjongs.recognizer

import org.scalatest.FunSuite

class RecognizerSpec extends FunSuite {

  val tile = read("../server/src/main/resources/public/tile.jpg", true)
  val hand = read("../server/src/main/resources/public/hand.jpg", true)

  test("A sample of tile image should be template") {
    assert(TemplateMatching.createTemplate(tile).isDefined)
  }

  test("A sample of hand image should have 14 tiles") {
    assert {
      TemplateMatching.createTemplate(tile).exists {
        case (templates, size) =>
          val (closed, open) = TemplateMatching.recognize(hand, templates, size.width.toInt, size.height.toInt)
          closed.size == 14 && open.isEmpty
      }
    }
  }

}
