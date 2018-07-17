package net.flatmap.cobra.lss

import java.io.File
import java.nio.file.{FileSystems, Files, Paths}

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import net.flatmap.cobra._
import net.flatmap.collaboration._
import play.api.libs.json.Json
import play.api.libs.json._

import com.dhpcs.jsonrpc._
import com.dhpcs.jsonrpc.JsonRpcMessage._

import scala.concurrent.duration._

object LanguageServerService extends LanguageService  {
  def props(env: Map[String,String]) = Props(classOf[LanguageServerService],env)
  def toPosition(from: Int, to: Int, content: String): Position = {
    val line = content.substring(0,from + 1).count(_ == "\n")
    var lineContent = content
    var tmpLength = 0
    for (i <- 0 until line) {
      tmpLength += lineContent.substring(0, lineContent.indexOf('\n')).length + 1
      lineContent = lineContent.substring(lineContent.indexOf('\n') + 1)
    }
    val index = from - tmpLength
    Position(line, index)
  }
}

class LanguageServerService(env: Map[String,String]) extends Actor with ActorLogging {
  implicit val dispatcher = context.system.dispatcher

  def receive = {
    case ResetSnippet(id, content, rev) =>
      log.info("create snippet")
      context.become(initialized(id,content,rev,sender()))
  }

  val files = collection.mutable.Map.empty[String,(String,ClientInterface[Char], String)]

  def initialized(id: String, initialContent: String, rev: Long, server: ActorRef): Receive = {
    lazy val editorInterface: EditorInterface[Char] = new EditorInterface[Char] {
      def applyOperation(operation: Operation[Char]) = {
        files.get(id).foreach { case (b,c, uri) =>
          val nc = Document(b).apply(operation).get.content.mkString
          files(id) = (nc,c, uri)
        }
      }

      def sendOperation(operation: Operation[Char], revision: Long) = {
        server ! Edit(id,operation,revision)
      }

      def applyAnnotations(aid: String, annotations: Annotations) = {
        // ignore annotations
      }

      def sendAnnotations(aid: String, annotations: Annotations, revision: Long) = {
        server ! Annotate(id, aid, annotations, revision)
      }
    }

    // Create temp directory and filesaver
    val workspacePath: String = Files.createTempDirectory(Paths.get(System.getProperty("java.io.tmpdir")), "cobra_ls_").normalize.toString ++ FileSystems.getDefault.getSeparator
    val saver = context.actorOf(FileSaver.props(workspacePath))

    val communicator = context.actorOf(LsCommunicator.props)

    //context.system.scheduler.scheduleOnce(5 seconds, communicator, Write("{\"id\":0,\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{\"capabilities\":{\"applyEdit\":true},\"rootPath\":\"/tmp/test\",\"trace\":\"off\"}}"))
    context.system.scheduler.scheduleOnce(5 seconds, communicator, InitializeParams(0, workspacePath, ClientCapabilities()))
    context.system.scheduler.scheduleOnce(5 seconds, communicator, Initialized())

    lazy val clientInterface = ClientInterface[Char](editorInterface)

    val fileUri = "file://" + workspacePath + id
    files += id -> (initialContent,clientInterface, fileUri)
    saver ! (initialContent, id)

    case object Refresh
    case object DelayRefresh
    var refreshDelay: Option[Cancellable] = None

    {
      case DelayRefresh =>
        refreshDelay.foreach(_.cancel())
        refreshDelay = Some(context.system.scheduler.scheduleOnce(.75 second, self, Refresh))
      case Refresh =>
        files.get(id).foreach {
          case (content, _, uri) =>
            saver ! (content, id)
            // bulk send saved edits to LS (maybe didClose -> didOpen)
            // Compile/getDiagnostics/apply edit
        }
        refreshDelay = None
      case AcknowledgeEdit(id2) if id == id2 => clientInterface.serverAck()
      case RemoteEdit(id2, op) if id == id2 =>
        clientInterface.remoteEdit(op)
        // save edits to bulk send to LS
        self ! DelayRefresh
      case RemoteAnnotations(id2, aid, as) if id == id2 => clientInterface.remoteAnnotations(aid, as)
      case CombinedRemoteEdit(id2, op, rev) if id == id2 => clientInterface.combinedRemoteEdit(op, rev)
      case RequestInfo(id2,from,to,guid) if id == id2 => communicator ! TextDocumentHoverRequest(TextDocumentPositionParams(TextDocumentIdentifier(fileUri), LanguageServerService.toPosition(from, to, files(id)._1))) // Get hover info
      case other => log.warning("unhandled message: " + other)
    }
  }
}
