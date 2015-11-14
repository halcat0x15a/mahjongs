package mahjongs

import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

import org.opencv.core._
import org.opencv.imgproc.Imgproc

package object recognizer {

  def findContours(mat: Mat): Buffer[MatOfPoint] = {
    val contours = Buffer.empty[MatOfPoint]
    Imgproc.findContours(mat, contours.asJava, new Mat, Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_TC89_KCOS)
    contours.sortBy(Imgproc.contourArea)(Ordering.Double.reverse)
  }

  def floodFill(mat: Mat, rows: Seq[Int], cols: Seq[Int]): Mat = {
    val mask = new Mat
    val color = new Scalar(0)
    for (row <- rows; col <- cols if mat.get(row, col)(0) > 0)
      Imgproc.floodFill(mat, mask, new Point(col, row), color)
    mat
  }

  def grid(mat: Mat, rows: Int, cols: Int): IndexedSeq[IndexedSeq[Mat]] = {
    val height = mat.rows / rows
    val width = mat.cols / cols
    for (row <- 0 until rows) yield {
      val rowRange = new Range(row * height, (row + 1) * height)
      for (col <- 0 until cols) yield
        mat.submat(rowRange, new Range(col * width, (col + 1) * width))
    }
  }

  def approxPoly(contour: MatOfPoint2f, epsilon: Double = 0.01): MatOfPoint2f = {
    val curve = new MatOfPoint2f
    Imgproc.approxPolyDP(contour, curve, Imgproc.arcLength(contour, true) * epsilon, true)
    if (curve.rows % 2 == 0)
      curve
    else
      approxPoly(contour, epsilon + 0.01)
  }

  def threshold(mat: Mat, inv: Boolean): Mat = {
    val (tpe, op) = if (inv) (Imgproc.THRESH_BINARY_INV, Imgproc.MORPH_OPEN) else (Imgproc.THRESH_BINARY, Imgproc.MORPH_CLOSE)
    Imgproc.threshold(mat, mat, 0, 255, tpe | Imgproc.THRESH_OTSU)
    Imgproc.morphologyEx(mat, mat, op, new Mat)
    mat
  }

  def crop(mat: Mat): Buffer[(Mat, MatOfPoint)] = {
    for (contour <- findContours(threshold(mat.clone, false))) yield {
      val patch = new Mat
      val rect = Imgproc.minAreaRect(new MatOfPoint2f(contour.toArray: _*))
      if (rect.angle < -45) {
        rect.angle += 90
        rect.size = new Size(rect.size.height, rect.size.width)
      }
      Imgproc.warpAffine(mat, patch, Imgproc.getRotationMatrix2D(rect.center, rect.angle, 1), mat.size)
      Imgproc.getRectSubPix(patch, rect.size, rect.center, patch)
      (patch, contour)
    }
  }

  def convexHull(contours: Seq[Point]): Rect = {
    val hull = new MatOfInt
    Imgproc.convexHull(new MatOfPoint(contours: _*), hull, false)
    Imgproc.boundingRect(new MatOfPoint(hull.toArray.map(contours): _*))
  }

  def flip(mat: Mat, code: Int): Mat = {
    val m = new Mat
    Core.flip(mat, m, code)
    m
  }

}
