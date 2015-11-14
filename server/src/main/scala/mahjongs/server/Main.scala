package mahjongs

package server

import com.twitter.conversions.storage._
import com.twitter.finagle.{Httpx, Service}
import com.twitter.finagle.httpx
import com.twitter.util.{Await, Future}

import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs

import mahjongs.recognizer.TemplateMatching

object Main extends App {
  System.load(getClass.getResource(s"/libopencv_java300.dylib").getPath)
  val train = Imgcodecs.imread(getClass.getResource("/train.jpg").getPath, Imgcodecs.CV_LOAD_IMAGE_GRAYSCALE)
  val matcher = TemplateMatching(train)
  //for (i <- 0 until 4) matcher.read(Imgcodecs.imread(getClass.getResource(s"/hand$i.jpg").getPath, Imgcodecs.CV_LOAD_IMAGE_GRAYSCALE))
  //matcher.read(Imgcodecs.imread(getClass.getResource(s"/hand5.jpg").getPath, Imgcodecs.CV_LOAD_IMAGE_GRAYSCALE))
  val service = new Service[httpx.Request, httpx.Response] {
    def apply(req: httpx.Request) =
      Future {
        println(req)
        val bytes = new Array[Byte](req.content.length)
        req.content.write(bytes, 0)
        val mat = Imgcodecs.imdecode(new MatOfByte(bytes: _*), Imgcodecs.CV_LOAD_IMAGE_GRAYSCALE)
        val indices = matcher.read(mat).flatMap(_._1)
        val tiles = indices.map(Tile.values).toList
        println(tiles)
        val m = Mahjongs(tiles.head, tiles.tail, Nil, false, false, false, East, East, 0)
        val hand = m.hands.head
        println(hand.fuList)
        println(hand.yakuList)
        println(hand.score)
        httpx.Response(req.version, httpx.Status.Ok)
      } rescue {
        case e =>
          println(e)
          Future.value(httpx.Response(req.version, httpx.Status.Ok))
      }
  }
  val server = Httpx.server.withMaxRequestSize(100.megabytes).serve(":8080", service)
  Await.ready(server)
}
