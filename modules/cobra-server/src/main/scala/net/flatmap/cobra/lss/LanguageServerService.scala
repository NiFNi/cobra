package net.flatmap.cobra.lss

import java.nio.file.{FileSystems, Files, Paths}

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import net.flatmap.cobra._
import net.flatmap.collaboration._

import scala.concurrent.duration._

object LanguageServerService extends LanguageService  {
  def props(env: Map[String,String]) = Props(classOf[LanguageServerService],env)
  def toPosition(from: Int, to: Int, content: String): Position = {
    val lines = List.empty ++ content.substring(0,from + 1).split("\r\n|\r|\n")
    val line = lines.length - 1
    var lineContent = content
    var tmpLength = 0
    for (i <- 0 until line) {
      tmpLength += lineContent.substring(0, lineContent.indexOf('\n')).length + 1
      lineContent = lineContent.substring(lineContent.indexOf('\n') + 1)
    }
    val index = from - tmpLength + 1
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

  val files = collection.mutable.Map.empty[String,(String,ClientInterface[Char], String, Long)]

  def initialized(id: String, initialContent: String, rev: Long, server: ActorRef): Receive = {
    lazy val editorInterface: EditorInterface[Char] = new EditorInterface[Char] {
      def applyOperation(operation: Operation[Char]) = {
        files.get(id).foreach { case (b,c, uri, version) =>
          val nc = Document(b).apply(operation).get.content.mkString
          files(id) = (nc,c, uri, version)
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
    val workspacePath: String = Files.createTempDirectory(Paths.get(
      System.getProperty("java.io.tmpdir")), "cobra_ls_").normalize
      .toString ++ FileSystems.getDefault.getSeparator
    val saver = context.actorOf(FileSaver.props(workspacePath))

    val communicator = context.actorOf(LsCommunicator.props)

    //context.system.scheduler.scheduleOnce(5 seconds, communicator, Write("{\"id\":0,\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{\"capabilities\":{\"applyEdit\":true},\"rootPath\":\"/tmp/test\",\"trace\":\"off\"}}"))
    //context.system.scheduler.scheduleOnce(5 seconds, communicator, InitializeParams(0, workspacePath, ClientCapabilities()))
    //context.system.scheduler.scheduleOnce(5 seconds, communicator, Initialized())

    lazy val clientInterface = ClientInterface[Char](editorInterface)

    val fileUri = "file://" + workspacePath + id + ".rb"
    files += id -> (initialContent, clientInterface, fileUri, 0.toLong)
    saver ! (initialContent, id + ".rb")
    communicator ! InitializeParams(123, "file://" + workspacePath, ClientCapabilities())
    context.system.scheduler.scheduleOnce(3 second, communicator, DidOpenTextDocumentParams(TextDocumentItem(fileUri, "ruby", 0, initialContent)))

    case object Refresh
    case object DelayRefresh
    var refreshDelay: Option[Cancellable] = None

    {
      case DelayRefresh =>
        refreshDelay.foreach(_.cancel())
        refreshDelay = Some(context.system.scheduler.scheduleOnce(.75 second, self, Refresh))
      case Refresh =>
        files.get(id).foreach {
          case (content, client, uri, version) =>
            val change = DidChangeTextDocumentParams(VersionedTextDocumentIdentifier(uri, version+1),
              Seq(TextDocumentContentChangeEvent(None, None, content)))
            files.update(id, (content, client, uri, version+1))
            communicator ! change
            saver ! (content, id + ".rb")
            val open = DidOpenTextDocumentParams(TextDocumentItem(uri, "ruby", 0, content))
        }
        refreshDelay = None
      case AcknowledgeEdit(id2) if id == id2 => clientInterface.serverAck()
      case RemoteEdit(id2, op) if id == id2 =>
        clientInterface.remoteEdit(op)
        log.info(op.toString)
        // save edits to bulk send to LS
        self ! DelayRefresh
      case RemoteAnnotations(id2, aid, as) if id == id2 => clientInterface.remoteAnnotations(aid, as)
      case CombinedRemoteEdit(id2, op, rev) if id == id2 => clientInterface.combinedRemoteEdit(op, rev)
      case RequestInfo(id2,from,to,guid) if id == id2 =>
        communicator ! (TextDocumentHoverRequest(TextDocumentPositionParams(
          TextDocumentIdentifier(fileUri), LanguageServerService.toPosition(from, to, files(id)._1))),
          guid, id, from, to)// Get hover info
      case (Hover(s: MarkUpContent, range), guid: String, id: String, from: Int, to: Int) => server ! Information(id, from, to, s.value, guid)
      case diags: Seq[Diagnostic] =>
        files.get(id).foreach {
          case (content, _, _, _) => {
            var result = new Annotations
            val lines = content.split("\n").map(_.length() + 1).toList
            var position = 0
            def offset(line: Int, ch: Int): Int =
              lines.take(line-1).reduceOption(_ + _).getOrElse(0) + ch
            diags.map({
              case Diagnostic(Range(s,_), seve, _, _, message) => (offset(s.line, s.character), seve, message)
            }).sortBy(_._1).foreach {
              case (o, s, m) =>
                if (o > position) {
                  result = result.plain(o - position)
                  position = o
                }
                s match {
                  case Some(DiagnosticSeverity.Error) =>
                    result = result.annotate(0, AnnotationOptions(messages = List(ErrorMessage(m))))
                  case Some(DiagnosticSeverity.Warning) =>
                    result = result.annotate(0, AnnotationOptions(messages = List(ErrorMessage(m))))
                  case Some(_) =>
                    result = result.annotate(0, AnnotationOptions(messages = List(ErrorMessage(m))))
                }
            }
            result.plain(content.length - position)
            clientInterface.localAnnotations("languageserver", result)
          }
        }
      case other => log.warning("unhandled message: " + other)
    }
  }
}
