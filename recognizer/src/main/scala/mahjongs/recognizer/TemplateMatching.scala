package mahjongs.recognizer

import org.opencv.core._
import org.opencv.imgproc.Imgproc

object TemplateMatching {

  val MaxResolution: Int = 1024 * 1024

  def createTemplate(mat: Mat): Option[(IndexedSeq[Mat], Size)] = {
    log("template_gray.png", mat)
    val fullBinary = mat.clone
    Imgproc.threshold(fullBinary, fullBinary, 0, 255, Imgproc.THRESH_BINARY | Imgproc.THRESH_OTSU)
    log("template_full_binary.png", fullBinary)
    crop(resize(mat, MaxResolution)).headOption.map {
      case (template, contour) =>
        mat.clone match {
          case tmp =>
            Imgproc.cvtColor(tmp, tmp, Imgproc.COLOR_GRAY2BGR)
            Imgproc.drawContours(tmp, java.util.Arrays.asList(contour), 0, new Scalar(0, 0, 255), 5)
            log("template_contour.png", tmp)
        }
        mat.clone match {
          case tmp =>
            Imgproc.cvtColor(tmp, tmp, Imgproc.COLOR_GRAY2BGR)
            val points = new Array[Point](4)
            minAreaRect(contour).points(points)
            for (i <- 0 until points.size) {
              Imgproc.line(tmp, points(i), points((i + 1) % points.size), new Scalar(255, 0, 0), 5)
            }
            log("template_rect.png", tmp)
        }
        log("template_crop.png", template)
        val mask = threshold(template.clone, true)
        log("template_binary.png", mask)
        val width = template.cols / 9
        val height = template.rows / 4
        floodFill(mask, (0 until 4).map(_ * height) :+ (mask.rows - 1), 0 until mask.cols)
        floodFill(mask, 0 until mask.rows, (0 until 9).map(_ * width) :+ (mask.cols - 1))
        Imgproc.dilate(mask, mask, new Mat)
        log("template_fill.png", mask)
        val rects = for (tiles <- grid(mask, 4, 9)) yield {
          for (tile <- tiles) yield {
            val contours = findContours(tile.clone).flatMap(_.toArray)
            if (contours.length > 0)
              Some(convexHull(contours))
            else
              None
          }
        }
        val size = rects.flatten.flatten.map(_.size).maxBy(_.area)
        val tiles = grid(template, 4, 9).zip(rects).flatMap {
          case (tiles, rects) =>
            tiles.zip(rects).map {
              case (tile, rect) =>
                Imgproc.getRectSubPix(tile, size, rect.fold(center(tile))(center), tile)
                tile
            }
        }.take(34)
        tiles.headOption.foreach(log("template_tile.png", _))
        (tiles, new Size(width, height))
    }
  }

  def recognize(mat: Mat, templates: Seq[Mat], width: Int, height: Int): Option[(Seq[Int], Seq[Seq[Int]])] =
    if (width > 0 && height > 0) {
      val result = crop(resize(mat, MaxResolution)).zipWithIndex.collect {
        case ((hand, contour), index) if hand.size.area > 0 =>
          Imgproc.resize(hand, hand, new Size(hand.size.width * height / hand.size.height, height))
          log("hand_crop.png", hand)
          val edge = approxPoly(new MatOfPoint2f(contour.toArray: _*)).rows
          val tiles = if (edge == 4) templates else templates ++ templates.map(m => flip(m.t, 0)) ++ templates.map(m => flip(m.t, 1))
          val tmp = hand.clone
          @annotation.tailrec
          def go(rects: List[(Int, Rect)]): List[(Int, Rect)] = {
            val locs = for (tile <- tiles) yield {
              val result = new Mat
              Imgproc.matchTemplate(hand, tile, result, Imgproc.TM_CCORR_NORMED)
              (tile, Core.minMaxLoc(result), result)
            }
            val ((tile, loc, result), i) = locs.zipWithIndex.maxBy(_._1._2.maxVal)
            Core.multiply(result, Scalar.all(255), result)
            log(s"matching_result${index}_${rects.size}.png", result)
            val rect = new Rect(loc.maxLoc, tile.size)
            log(s"hand_rect${index}_${rects.size}.png", tmp)
            log(s"hand${index}_${rects.size}.png", hand)
            if (rects.forall(pair => !intersects(rect, pair._2))) {
              Imgproc.rectangle(tmp, rect.tl, rect.br, new Scalar(0), 5)
              Imgproc.rectangle(hand, rect.tl, rect.br, new Scalar(0), -1)
              go((i % 34, rect) :: rects)
            } else {
              rects
            }
          }
          val indices = go(Nil).sortBy(_._2.x).map(_._1)
          (edge == 4 && indices.size != 4, indices)
      }.groupBy(_._1).mapValues(_.map(_._2))
      result(true).headOption.map((_, result.get(false).toList.flatten))
    } else {
      None
    }

}
