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

import spray.json._, DefaultJsonProtocol._

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

  case class CalculationData(data: RecognitionResult, dealer: Boolean, selfdrawn: Boolean, seat: Int, round: Int, dora: Int)
  implicit val calculationDataFormat = jsonFormat6(CalculationData)

  case class CalculationResult(han: Int, fu: Int, yaku: List[String], point: String)
  implicit val calculationResultFormat = jsonFormat4(CalculationResult)

  def decode(data: String): Array[Byte] =
    Base64.getMimeDecoder.decode(data.substring(data.indexOf(',') + 1))

  loadLibrary()

  implicit val system: ActorSystem = ActorSystem()

  implicit val materializer: ActorMaterializer = ActorMaterializer()

  import system.dispatcher

  val route =
    path("") {
      redirect("index.html", StatusCodes.MovedPermanently)
    } ~ path("train") {
      entity(as[TrainingData]) {
        case TrainingData(data) =>
          complete {
            TemplateMatching.createTemplate(toMat(decode(data), true)).map {
              case (images, size) =>
                TrainingResult(
                  images.map(image => Base64.getMimeEncoder.encodeToString(fromMat(image)))(collection.breakOut),
                  size.width.toInt,
                  size.height.toInt
                )
            }
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
        case CalculationData(RecognitionResult(closed, open), dealer, selfdrawn, seat, round, dora) =>
        complete {
          val tiles = closed.map(Tile.values)
          val openMelds = open.flatMap { indices =>
            if (indices.size == 4 && indices(0) == 31 && indices(3) == 31 && indices(1) == indices(2))
              Some(Kantsu(Tile.values(indices(1)), true))
            else
              Meld.parse(indices.map(Tile.values), false)
          }
          Hand.calc(tiles.last, tiles.init, openMelds, Situation(dealer, selfdrawn, Wind.values(seat), Wind.values(round), dora)).map { hand =>
            val detail = hand.win match {
              case Ron(_) => ""
              case NonDealerTsumo(dealer, others) => s"($others/$dealer)"
              case DealerTsumo(others) => s"($others)"
            }
            CalculationResult(hand.han, hand.fu, hand.yaku.map(_.name)(collection.breakOut), s"${hand.win.points}$detail")
          }
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
