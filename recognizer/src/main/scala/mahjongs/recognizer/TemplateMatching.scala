package mahjongs.recognizer

import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc

object TemplateMatching {

  val MaxResolution: Int = 1024 * 1024

  def createTemplate(mat: Mat): (IndexedSeq[Mat], Size) = {
    val template = crop(resize(mat, MaxResolution)).head._1
    Imgcodecs.imwrite("template.png", template)
    val mask = threshold(template.clone, true)
    val width = template.cols / 9
    val height = template.rows / 4
    floodFill(mask, (0 until 4).map(_ * height) :+ (mask.rows - 1), 0 until mask.cols)
    floodFill(mask, 0 until mask.rows, (0 until 9).map(_ * width) :+ (mask.cols - 1))
    Imgproc.dilate(mask, mask, new Mat)
    Imgcodecs.imwrite("template_mask.png", mask)
    val rects = for (tiles <- grid(mask, 4, 9)) yield
      for (tile <- tiles) yield {
        val contours = findContours(tile.clone).flatMap(_.toArray)
        if (contours.length > 0)
          Some(convexHull(contours))
        else
          None
      }
    val size = rects.flatten.flatten.map(_.size).maxBy(_.area)
    val tiles = grid(template, 4, 9).zip(rects).flatMap {
      case (tiles, rects) =>
        tiles.zip(rects).map {
          case (tile, rect) =>
            Imgproc.getRectSubPix(tile, size, rect.fold(center(tile))(center), tile)
            tile
        }
    }.take(35)
    (tiles, new Size(width, height))
  }

  def recognize(mat: Mat, templates: Seq[Mat], width: Int, height: Int): (Seq[Int], Seq[Seq[Int]]) = {
    val result = crop(resize(mat, MaxResolution)).collect {
      case (hand, contour) if hand.size.area > 0 =>
        Imgproc.resize(hand, hand, new Size(hand.size.width * height / hand.size.height, height))
        Imgcodecs.imwrite(s"hand.png", hand)
        val edge = approxPoly(new MatOfPoint2f(contour.toArray: _*)).rows
        val tiles = templates ++ templates.map(m => flip(m.t, 0)) ++ templates.map(flip(_, -1)) ++ templates.map(m => flip(m.t, 1))
        def go(rects: List[(Int, Rect)]): List[(Int, Rect)] = {
          val locs = for (tile <- tiles) yield {
            val result = new Mat
            Imgproc.matchTemplate(hand, tile, result, Imgproc.TM_CCORR_NORMED)
            (tile, Core.minMaxLoc(result))
          }
          val ((tile, loc), i) = locs.zipWithIndex.maxBy(_._1._2.maxVal)
          val rect = new Rect(loc.maxLoc, tile.size)
          if (rects.forall(pair => !intersects(rect, pair._2))) {
            Imgproc.rectangle(hand, rect.tl, rect.br, new Scalar(0), -1)
            println(i, loc.maxVal)
            go((i % 35, rect) :: rects)
          } else {
            Imgcodecs.imwrite("hand_result.png", hand)
            rects
          }
        }
        val indices = go(Nil).sortBy(_._2.x).map(_._1)
        (edge == 4 && !indices.contains(34), indices)
    }.groupBy(_._1).mapValues(_.map(_._2))
    (result(true)(0), result.get(false).toList.flatten)
  }

}
