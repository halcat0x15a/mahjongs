package mahjongs.server

import java.util.Base64

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model._
import akka.util.ByteString

import spray.json._, DefaultJsonProtocol._

import org.opencv.core._

import mahjongs.solver._
import mahjongs.recognizer._

object Main extends App {

  case class TrainingData(data: String)
  implicit val trainingDataFormat = jsonFormat1(TrainingData)

  case class TrainingResult(image: List[String], width: Int, height: Int)
  implicit val trainingResultFormat = jsonFormat3(TrainingResult)

  case class RecognitionData(data: String, training: TrainingResult)
  implicit val recognitionDataFormat = jsonFormat2(RecognitionData)

  case class RecognitionResult(closed: List[Int], open: List[List[Int]])
  implicit val recognitionResultFormat = jsonFormat2(RecognitionResult)

  case class CalculationData(data: RecognitionResult, dealer: Boolean, selfdrawn: Boolean, lastchance: Boolean, seat: Int, prevailing: Int, dora: Int)
  implicit val calculationDataFormat = jsonFormat7(CalculationData)

  case class CalculationResult(han: Int, fu: Int, yaku: List[String], point: String)
  implicit val calculationResultFormat = jsonFormat4(CalculationResult)

  def decode(data: String): Array[Byte] =
    Base64.getMimeDecoder.decode(data.substring(data.indexOf(',') + 1))

  System.load(getClass.getResource(s"/libopencv_java300.dylib").getPath)

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  import system.dispatcher

  val route =
    path("") {
      redirect("index.html", StatusCodes.MovedPermanently)
    } ~ path("train") {
      entity(as[TrainingData]) {
        case TrainingData(data) =>
          val (images, size) = TemplateMatching.createTemplate(toMat(decode(data), true))
          complete {
            TrainingResult(
              images.map(image => Base64.getMimeEncoder.encodeToString(fromMat(image)))(collection.breakOut),
              size.width.toInt,
              size.height.toInt
            )
          }
      }
    } ~ path("recognize") {
      entity(as[RecognitionData]) {
        case RecognitionData(data, TrainingResult(trainingData, width, height)) =>
          val (closed, open) = TemplateMatching.recognize(toMat(decode(data), true), trainingData.map(data => toMat(decode(data), true)), width, height)
          complete {
            RecognitionResult(closed.toList, open.map(_.toList)(collection.breakOut))
          }
      }
    } ~ path("calculate") {
      entity(as[CalculationData]) {
        case CalculationData(RecognitionResult(closed, open), dealer, selfdrawn, lastchance, seat, prevailing, dora) =>
        complete {
          val tiles = closed.map(Tile.values)
          val openTiles = open.flatMap { indices =>
            if (indices.size == 4 && indices.contains(34))
              Meld.parse(indices.find(_ != 34).toList.flatMap(index => List.fill(4)(Tile.values(index))), true)
            else
              Meld.parse(indices.map(Tile.values), false)
          }
          val hand = Hand.calc(tiles.last, tiles.init, openTiles, dealer, selfdrawn, Tile.wind(seat), Tile.wind(prevailing), dora)
          val detail = hand.score match {
            case Ron(_) => ""
            case NonDealerTsumo(dealer, others) => s"($others/$dealer)"
            case DealerTsumo(others) => s"($others)"
          }
          CalculationResult(hand.han, hand.fu, hand.yaku.map(_.name)(collection.breakOut), s"${hand.score.points}$detail")
        }
      }
    } ~ pathPrefix("") {
      encodeResponse {
        getFromResourceDirectory("public")
      }
    }

  val binding = Http().bindAndHandle(route, "0.0.0.0", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  scala.io.StdIn.readLine()
  binding.flatMap(_.unbind()).onComplete(_ => system.shutdown())
  //Await.ready(binding, Duration.Inf)

}
