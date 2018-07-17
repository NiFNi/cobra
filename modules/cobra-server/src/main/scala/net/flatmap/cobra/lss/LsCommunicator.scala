package net.flatmap.cobra.lss

import java.io.{InputStream, OutputStream, PrintWriter}

import scala.concurrent.duration._
import akka.actor.{Actor, ActorLogging, PoisonPill, Props}
import play.api.libs.json._

import scala.concurrent.ExecutionContext
import scala.sys.process._
import scala.util.{Failure, Success, Try}
import com.dhpcs.jsonrpc._
import com.dhpcs.jsonrpc.JsonRpcMessage._
import net.flatmap.cobra.lss.JsonRpcUtils.valueFormat

/**
  * Created by nif on 05.03.18.
  */
class LsCommunicator extends Actor with ActorLogging {
  implicit val dispatcher = context.system.dispatcher
  implicit val ec: ExecutionContext = ExecutionContext.global
  val server: ProcessBuilder = Process("dartls")
  var curId = 0


  val pio = new ProcessIO(writer, reader, error)
  // Start process
  server.run(pio)

  // send outputstream to itself to change the context
  def writer(output: OutputStream) = {
    self ! output
  }

  // start reader thread
  def reader(input: InputStream) = {
    /* TODO move to future */
    val t = new InputStreamThread(input, self)
    new Thread(t).start()
  }

  def error(input: InputStream) = {
    /* TODO move to future */
    val t = new InputStreamThread(input, self)
    new Thread(t).start()
  }

  override def receive: Receive = {
    case out: OutputStream =>
      context.become(initialized(out))
    case a =>
      log.warning("got something not initialized")
      context.system.scheduler.scheduleOnce(1 second, self, a)

  }

  def initialized(outputStream: OutputStream): Receive = {
    {
      case command: TextDocumentDefinitionRequest =>
        //implicit val positionParamsFormat = Json.using[Json.WithDefaultValues].format[TextDocumentPositionParams]
        //implicit val format: Format[TextDocumentDefinitionRequest] = valueFormat(TextDocumentDefinitionRequest)(_.params)
        log.info("got definitionrequest to write")
        log.info(command.toString)
        val server = ServerCommand.write(command, NumericCorrelationId(curId))
        log.info("a")
        val json = Json.toJson(server)
        log.info("b")
        val a: String = Json.stringify(json)
        log.info("c")
        curId += 1
        val msg: String = "Content-Length: " + a.length() + "\r\n" +
          "Content-Type: application/vscode-jsonrpc; charset=utf8" + "\r\n\r\n" + a + "\r\n"
        log.info("Writer: " + msg)
        val writer = new PrintWriter(outputStream)
        writer.println(msg)
        writer.flush()
        log.info("writer: " + a)
      case command: ServerCommand =>
        implicit val positionParamsFormat = Json.using[Json.WithDefaultValues].format[TextDocumentPositionParams]
        implicit val format: Format[TextDocumentDefinitionRequest] = valueFormat(TextDocumentDefinitionRequest)(_.params)
        log.info("got servercommand to write")
        log.info(command.toString)
        val server = ServerCommand.write(command, NumericCorrelationId(curId))
        log.info("a")
        val json = Json.toJson(server)
        log.info("b")
        val a: String = Json.stringify(json)
        log.info("c")
        curId += 1
        val msg: String = "Content-Length: " + a.length() + "\r\n" +
          "Content-Type: application/vscode-jsonrpc; charset=utf8" + "\r\n\r\n" + a + "\r\n"
        log.info("Writer: " + msg)
        val writer = new PrintWriter(outputStream)
        writer.println(msg)
        writer.flush()
        log.info("writer: " + a)

      case notification: Notification =>
        log.info("got notification to write")
        val a: String = Json.stringify(Json.toJson(Notification.write(notification)))
        val msg: String = "Content-Length: " + a.length() + "\r\n" +
          "Content-Type: application/vscode-jsonrpc; charset=utf8" + "\r\n\r\n" + a + "\r\n"
        log.info("Writer: " + msg)
        val writer = new PrintWriter(outputStream)
        writer.println(msg)
        writer.flush()
        log.info("writer: " + a)

      case Read(a) =>
        // Try to read message
        //log.info("read: " + a)
        Try(Json.parse(a)) match {
          case Failure(exception) => log.error(exception.toString)
          case Success(json) =>
            log.info(json.toString())
            Json.fromJson[JsonRpcMessage](json).fold({ errors =>
              Left()
            }, Right(_)) match {
              case Left => log.error("error in json parsing")
              case Right(message) => message match {
                // Notification handling
                case notification: JsonRpcNotificationMessage => log.info("received notification")
                  log.info(notification.toString)
                // Request handling
                case request: JsonRpcRequestMessage =>
                  log.info("received request")
                  log.info(ServerCommand.read(request).toString)
                // Response handling
                case response: JsonRpcResponseMessage => response match {
                  case success: JsonRpcResponseSuccessMessage =>
                    log.info("received success")
                    log.info(success.toString)
                    //log.info(ResultResponse.read(success).toString)
                  case error: JsonRpcResponseErrorMessage =>
                    log.error(error.toString)
                }
              }
            }

        }
      case a => log.info("got something else")
    }
  }
}

object LsCommunicator {
  def props = Props(new LsCommunicator())
}

case class Write(toWrite: String)

case class Read(wasRead: String)