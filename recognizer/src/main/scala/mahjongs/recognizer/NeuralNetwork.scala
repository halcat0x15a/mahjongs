package mahjongs.recognizer

import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc
import org.opencv.features2d._
import org.opencv.ml._

import scala.collection.JavaConverters._

class NeuralNetwork(samples: Seq[Mat]) {

  val Attributes: Int = 16 * 16
  val HiddenLayer: Int = 16
  val Classes: Int = 34

  val network: ANN_MLP = ANN_MLP.create
  network.setLayerSizes(new MatOfInt(Attributes, HiddenLayer, Classes))
  network.setActivationFunction(ANN_MLP.SIGMOID_SYM, 0.6, 1)
  network.setTrainMethod(ANN_MLP.BACKPROP, 0.1, 0.1)
  network.setTermCriteria(new TermCriteria(TermCriteria.COUNT | TermCriteria.EPS, 1000, 0.000001))

  def _vector(mat: Mat): Mat = {
    Imgproc.threshold(mat, mat, 0, 255, Imgproc.THRESH_BINARY_INV | Imgproc.THRESH_OTSU)
    Imgproc.dilate(mat, mat, new Mat, new Point, 1)
    Imgproc.resize(mat, mat, new Size(16, 16), 0, 0, Imgproc.INTER_LINEAR)
    Imgproc.threshold(mat, mat, 0, 255, Imgproc.THRESH_BINARY | Imgproc.THRESH_OTSU)
    //Imgcodecs.imwrite(s"sample${mat.hashCode}.png", mat)
    Core.normalize(mat, mat, 0, 1, Core.NORM_MINMAX)
    mat.reshape(1, 1).convertTo(mat, CvType.CV_32F)
    mat
  }

  def __vector(mat: Mat): Mat = {
    Imgproc.resize(mat, mat, new Size(128, 128), 0, 0, Imgproc.INTER_LINEAR)
    val input = mat.clone
    Imgproc.threshold(mat, mat, 0, 255, Imgproc.THRESH_BINARY_INV | Imgproc.THRESH_OTSU)
    val contours = new java.util.ArrayList[MatOfPoint]
    Imgproc.findContours(mat, contours, new Mat, Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_TC89_KCOS)
    for (i <- 0 until contours.size)
      Imgproc.drawContours(input, contours, i, new Scalar(255, 0, 0))
    val contourMat = new MatOfPoint(contours.asScala.flatMap(_.toArray): _*)
    val hull = new MatOfInt
    Imgproc.convexHull(contourMat, hull, false)
    Imgproc.drawContours(input, java.util.Arrays.asList(new MatOfPoint(hull.toArray.map(n => contourMat.toList.get(n)): _*)), 0, new Scalar(255, 0, 0))
    Imgcodecs.imwrite(s"sample${input.hashCode}.png", input)
    mat
  }

  def ___vector(mat: Mat): Mat = {
    Imgproc.resize(mat, mat, new Size(256, 256), 0, 0, Imgproc.INTER_LINEAR)
    Imgproc.threshold(mat, mat, 0, 255, Imgproc.THRESH_BINARY_INV | Imgproc.THRESH_OTSU)
    Imgproc.erode(mat, mat, new Mat, new Point(-1,-1), 1)
    Imgproc.dilate(mat, mat, new Mat, new Point(-1,-1), 2)
    Imgproc.erode(mat, mat, new Mat, new Point(-1,-1), 1)
    Imgproc.morphologyEx(mat, mat, Imgproc.MORPH_CLOSE, new Mat, new Point(-1,-1), 2)
    Imgproc.morphologyEx(mat, mat, Imgproc.MORPH_OPEN, new Mat, new Point(-1,-1), 2)
    Imgcodecs.imwrite(s"sample${mat.hashCode}.png", mat)
    mat
  }

  def vector(mat: Mat): Mat = {
    val keypoints = new MatOfKeyPoint
    val detector = FeatureDetector.create(FeatureDetector.AKAZE)
    Imgproc.resize(mat, mat, new Size(128, 128), 0, 0, Imgproc.INTER_LINEAR)
    Imgproc.threshold(mat, mat, 0, 255, Imgproc.THRESH_BINARY_INV | Imgproc.THRESH_OTSU)
    Imgproc.erode(mat, mat, new Mat, new Point(-1,-1), 1)
    Imgproc.dilate(mat, mat, new Mat, new Point(-1,-1), 2)
    Imgproc.erode(mat, mat, new Mat, new Point(-1,-1), 1)
    detector.detect(mat, keypoints)
    Features2d.drawKeypoints(mat, keypoints, mat)
    Imgcodecs.imwrite(s"sample${mat.hashCode}.png", mat)
    mat
  }

  val inputs: Mat = new Mat
  for (sample <- samples) {
    inputs.push_back(vector(sample))
  }

  val outputs: Mat = Mat.eye(Classes, Classes, CvType.CV_32F)
  network.train(inputs, Ml.ROW_SAMPLE, outputs)

  def predict(mat: Mat): MatOfFloat = {
    val result = new MatOfFloat
    network.predict(vector(mat), result, StatModel.RAW_OUTPUT)
    result
  }

}
