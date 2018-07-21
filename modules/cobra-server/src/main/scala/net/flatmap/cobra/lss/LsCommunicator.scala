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

import scala.collection.mutable
import scala.collection.mutable.Map

/**
  * Created by nif on 05.03.18.
  */
class LsCommunicator extends Actor with ActorLogging {
  implicit val dispatcher = context.system.dispatcher
  implicit val ec: ExecutionContext = ExecutionContext.global
  val server: ProcessBuilder = Process("solargraph stdio")
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
    //val t = new InputStreamThread(input, self)
    //new Thread(t).start()
  }

  override def receive: Receive = {
    case out: OutputStream =>
      context.become(initialized(out))
    case a =>
      log.warning("got something not initialized")
      context.system.scheduler.scheduleOnce(1 second, self, a)

  }

  def initialized(outputStream: OutputStream): Receive = {
    val waiting: mutable.Map[CorrelationId, (ServerCommand, String, String, Int, Int)] = mutable.Map.empty

    {
      case (command: TextDocumentHoverRequest, guid: String, id: String, from: Int, to: Int) =>
        implicit val positionParamsFormat = Json.using[Json.WithDefaultValues].format[TextDocumentPositionParams]
        val s: JsValue = Json.toJson(command.params)
        val rpcMessage = JsonRpcRequestMessage("textDocument/hover", s.as[JsObject], NumericCorrelationId(curId))
        implicit val format: Format[TextDocumentDefinitionRequest] = valueFormat(TextDocumentDefinitionRequest)(_.params)
        log.info("got servercommand to write")
        log.info(command.toString)
        //val server: JsonRpcMessage = ServerCommand.write(command, NumericCorrelationId(curId))
        val json = Json.toJson(rpcMessage)
        val a: String = Json.stringify(json)
        waiting += (NumericCorrelationId(curId) -> (command, guid, id, from, to))
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
        val json = Json.toJson(server)
        val a: String = Json.stringify(json)
        waiting += (NumericCorrelationId(curId) -> (command, "", "", 0, 0))
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
                  Notification.read(notification).get match {
                    case d: PublishDiagnostics => context.parent ! d.diagnostics
                  }
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
                    waiting.get(success.id).foreach { command =>
                      command match {
                        case (_: InitializeParams, _, _, _, _) => self ! Initialized()
                        case (_: TextDocumentHoverRequest, guid, id, from, to) =>
                          context.parent ! (ResultResponse.read(success, "textDocument/hover").get, guid, id, from, to)
                        case c => log.warning("got answer to unknown response" + c)
                      }
                      waiting -= success.id

                    }
                    //log.info(ResultResponse.read(success, success.).toString)
                  case error: JsonRpcResponseErrorMessage =>
                    log.error(error.toString)
                }
              }
            }

        }
      case a => log.info("got something else" + a)
    }
  }
}

object LsCommunicator {
  def props = Props(new LsCommunicator())
}

case class Write(toWrite: String)

case class Read(wasRead: String)