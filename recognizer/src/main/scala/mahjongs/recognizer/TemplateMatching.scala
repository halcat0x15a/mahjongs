package mahjongs.recognizer

import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc

case class TemplateMatching(trainingData: Mat) {

  lazy val template = crop(lowResolution(trainingData, TemplateMatching.MaxResolution)).head._1

  lazy val width = template.cols / 9

  lazy val height = template.rows / 4

  lazy val tiles = {
    val mask = threshold(template.clone, true)
    floodFill(mask, (0 until 4).map(_ * height) :+ (mask.rows - 1), 0 until mask.cols)
    floodFill(mask, 0 until mask.rows, (0 until 9).map(_ * width) :+ (mask.cols - 1))
    Imgproc.dilate(mask, mask, new Mat)
    Imgcodecs.imwrite("train_mask.png", mask)
    val rects = for (tiles <- grid(mask, 4, 9)) yield
      for (tile <- tiles) yield {
        val contours = findContours(tile.clone).flatMap(_.toArray)
        if (contours.length > 0) {
          Some(convexHull(contours))
        } else {
          None
        }
      }
    val maxSize = rects.flatten.flatten.map(_.size).maxBy(_.area)
    val allTile = for {
      (tiles, rects) <- grid(template, 4, 9) zip rects
      (tile, rect) <- tiles zip rects
    } yield {
      val point = rect.fold(new Point(tile.cols / 2, tile.rows / 2))(rect => new Point(rect.x + rect.width / 2, rect.y + rect.height / 2))
      Imgproc.getRectSubPix(tile, maxSize, point, tile)
      tile
    }
    val tiles = allTile.take(34).toVector
    tiles ++ tiles.map(flip(_, 0)) ++ tiles.map(m => flip(m.t, 0)) ++ tiles.map(m => flip(m.t, 1))
  }

  def lowResolution(input: Mat, maxResolution: Int): Mat =
    if (input.total <= maxResolution) {
      input
    } else {
      Imgproc.pyrDown(input, input)
      lowResolution(input, maxResolution)
    }

  val names = Vector("萬", "筒", "索").flatMap(suit => Vector("一", "二", "三", "四", "五", "六", "七", "八", "九").map(n => s"$n$suit")) ++ Vector("東", "南", "西", "北", "白", "發", "中")

  def read(input: Mat) = {
    for ((hand, contour) <- crop(lowResolution(input, TemplateMatching.MaxResolution)) if hand.size.area > 0) yield {
      Imgproc.resize(hand, hand, new Size(hand.size.width * height / hand.size.height, height))
      Imgcodecs.imwrite("hand.png", hand)
      val edge = approxPoly(new MatOfPoint2f(contour.toArray: _*)).rows
      val indices = for (n <- 0 until hand.cols / width) yield {
        val locs = for (tile <- tiles) yield {
          val result = new Mat
          Imgproc.matchTemplate(hand, tile, result, Imgproc.TM_CCORR_NORMED)
          (tile, Core.minMaxLoc(result))
        }
        val ((tile, loc), i) = locs.zipWithIndex.maxBy(_._1._2.maxVal)
        println(names(i % 34), loc.maxVal)
        Imgproc.rectangle(hand, loc.maxLoc, new Point(loc.maxLoc.x + tile.cols, loc.maxLoc.y + tile.rows), new Scalar(0), -1)
        Imgcodecs.imwrite(s"result$n.png", hand)
        i % 34
      }
      (indices, edge == 4)
    }
  }

}

object TemplateMatching {

  val MaxResolution: Int = 2048 * 2048

}
