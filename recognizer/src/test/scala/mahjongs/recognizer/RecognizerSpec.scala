package mahjongs.recognizer

import java.nio.file.{Paths, Files}
import java.util.ArrayList

import scala.collection.JavaConverters._

import org.scalatest.FunSpec

import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc
import org.opencv.utils.Converters

class RecognizerSpec extends FunSpec {

  System.load(getClass.getResource(s"/libopencv_java300.dylib").getPath)

  describe("Recognizer") {
    val images = Files.list(Paths.get(getClass.getResource(s"/train").toURI)).iterator.asScala.map(path => Imgcodecs.imread(path.toString, Imgcodecs.CV_LOAD_IMAGE_GRAYSCALE)).toVector
    new NeuralNetwork(images ++ Files.list(Paths.get(getClass.getResource(s"/sample").toURI)).iterator.asScala.map(path => Imgcodecs.imread(path.toString, Imgcodecs.CV_LOAD_IMAGE_GRAYSCALE)))
    val nn = new NeuralNetwork(images)
    for (sample <- Files.list(Paths.get(getClass.getResource(s"/sample").toURI)).iterator.asScala.map(path => Imgcodecs.imread(path.toString, Imgcodecs.CV_LOAD_IMAGE_GRAYSCALE)))
      println(nn.predict(sample).toArray.zipWithIndex.maxBy(_._1))
  }

}
