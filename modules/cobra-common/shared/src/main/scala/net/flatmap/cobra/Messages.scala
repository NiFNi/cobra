package net.flatmap.cobra

import java.nio.ByteBuffer

import boopickle.Default._
import net.flatmap.collaboration._

import scala.util.matching.Regex

sealed trait ClientMessage
sealed trait ServerMessage
sealed trait SnippetMessage { val id: String }

case object HeartBeat extends ClientMessage with ServerMessage

case class WatchFile(path: String) extends ClientMessage
case class FileUpdate(path: String) extends ServerMessage

case class InitDoc(id: String, file: String, mode: Mode) extends ClientMessage with SnippetMessage
case class Edit(id: String, operation: Operation[Char], revision: Long) extends ClientMessage with SnippetMessage
case class Annotate(id: String, aid: String, annotations: Annotations, revision: Long) extends ClientMessage with SnippetMessage

case class Sendback(id: String, props: Map[String,String], s: String) extends ClientMessage with ServerMessage with SnippetMessage

case class RequestInfo(id: String, from: Int, to: Int, uid: String) extends ClientMessage with ServerMessage with SnippetMessage
case class Information(id: String, from: Int, to: Int, body: String, uid: String) extends ClientMessage with ServerMessage with SnippetMessage

case class AcknowledgeEdit(id: String) extends ServerMessage with SnippetMessage
case class RemoteEdit(id: String, op: Operation[Char]) extends ServerMessage with SnippetMessage
case class RemoteAnnotations(id: String, aid: String, annotations: Annotations) extends ServerMessage with SnippetMessage
case class CombinedRemoteEdit(id: String, op: Operation[Char], revisions: Long) extends ServerMessage with SnippetMessage
case class ResetSnippet(id: String, content: String, revision: Long) extends ServerMessage with SnippetMessage

case class RevealOptionsUpdate(values: Map[String,String]) extends ServerMessage
case class TitleUpdate(newTitle: String) extends ServerMessage
case class ThemeUpdate(code: String, slides: String) extends ServerMessage
case class LanguageUpdate(newLang: String) extends ServerMessage
case class SnippetChanged(src: String) extends ServerMessage

// TODO: This is a temporary hack until snippet management is implemented on the server.
case object ResetAllSnippets extends ClientMessage

trait Picklers {
  implicit val charActionPickler: Pickler[Action[Char]] =
    compositePickler[Action[Char]]
      .addConcreteType[Retain]
      .addConcreteType[Insert[Char]]
      .addConcreteType[Delete]

  implicit val annotationPickler: Pickler[Annotation] =
    compositePickler[Annotation]
      .addConcreteType[Empty]
      .addConcreteType[Annotated]

  implicit val modePickler: Pickler[Mode] =
    compositePickler[Mode]
      .addConcreteType[Scala.type]
      .addConcreteType[Isabelle.type]
      .addConcreteType[Plain.type]
      .addConcreteType[Haskell.type]
      .addConcreteType[LanguageServer.type]

  implicit val annotationMessagePickler: Pickler[AnnotationMessage] =
    compositePickler[AnnotationMessage]
      .addConcreteType[ErrorMessage]
      .addConcreteType[WarningMessage]
      .addConcreteType[InfoMessage]
      .addConcreteType[OutputMessage]
      .addConcreteType[StateMessage]
}

object ClientMessage extends Picklers {
  def read(bytes: ByteBuffer): ClientMessage = Unpickle[ClientMessage].fromBytes(bytes)
  def write(message: ClientMessage): ByteBuffer = Pickle.intoBytes(message)
}

object ServerMessage extends Picklers {
  def read(bytes: ByteBuffer): ServerMessage = Unpickle[ServerMessage].fromBytes(bytes)
  def write(message: ServerMessage): ByteBuffer = Pickle.intoBytes(message)
}