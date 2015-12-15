package mahjongs

import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc

package object recognizer {

  for {
    ext <- sys.props("os.name").toLowerCase match {
      case name if name.contains("nix") => Some("so")
      case name if name.contains("mac") => Some("dylib")
      case _ => None
    }
  } System.load(getClass.getResource(s"/libopencv_java300.$ext").getPath)

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
      Imgproc.warpAffine(mat, patch, Imgproc.getRotationMatrix2D(rect.center, rect.angle, 1), mat.size)
      Imgproc.getRectSubPix(patch, rect.size, rect.center, patch)
      if (rect.angle <= -45) Core.flip(patch.t, patch, 0)
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

  def resize(mat: Mat, max: Int): Mat = {
    val r = math.sqrt(mat.rows * mat.cols / max.toDouble)
    if (r > 1) Imgproc.resize(mat, mat, new Size(mat.size.width / r, mat.size.height / r))
    mat
  }

  def center(mat: Mat): Point =
    new Point(mat.size.width / 2, mat.size.height / 2)

  def center(rect: Rect): Point =
    new Point(rect.x + rect.width / 2, rect.y + rect.height / 2)

  def toMat(bytes: Array[Byte], gray: Boolean): Mat =
    Imgcodecs.imdecode(new MatOfByte(bytes: _*), if (gray) Imgcodecs.CV_LOAD_IMAGE_GRAYSCALE else Imgcodecs.CV_LOAD_IMAGE_COLOR)

  def fromMat(mat: Mat): Array[Byte] = {
    val buf = new MatOfByte
    Imgcodecs.imencode(".png", mat, buf)
    buf.toArray
  }

  def intersects(a: Rect, b: Rect): Boolean =
    math.max(a.x, b.x) < math.min(a.x + a.width, b.x + b.width) && math.max(a.y, b.y) < math.min(a.y + a.height, b.y + b.height)

  def read(filename: String, gray: Boolean): Mat =
    Imgcodecs.imread(filename, if (gray) Imgcodecs.IMREAD_GRAYSCALE else Imgcodecs.IMREAD_COLOR)

}
