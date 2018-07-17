package net.flatmap.cobra

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import net.flatmap.cobra.ghc.HaskellService
import net.flatmap.cobra.isabelle.IsabelleService
import net.flatmap.cobra.lss.LanguageServerService
import net.flatmap.cobra.scalac.ScalaService
import net.flatmap.collaboration.{Annotations, Document, Server}

import scala.util.{Failure, Success}

object SnippetServer {
  def props(env: Map[String,String]) = Props(classOf[SnippetServer],env)

  val services: Map[Mode,LanguageService] = Map(
    Isabelle -> IsabelleService,
    Scala -> ScalaService,
    Haskell -> HaskellService,
    LanguageServer -> LanguageServerService
  )
}

/**
  * Created by martin on 10.05.16.
  */
class SnippetServer(env: Map[String,String]) extends Actor with ActorLogging {
  def receive = {
    case InitDoc(id,content,mode) =>
      val service = SnippetServer.services.get(mode).map { ls =>
        val src = context.actorOf(ls.props(env),mode.name)
        log.info("mode: " + mode.name)
        src ! ResetSnippet(id,content,0)
        src
      }
      context.become(initialized(mode, new Server(Document(content)), Set(sender) ++ service,Map.empty))
      context.watch(sender)
  }

  def initialized(mode: Mode, server: Server[Char], listeners: Set[ActorRef], annotations: Map[String,Annotations]): Receive = {
    case Terminated(listener) =>
      log.debug("client disconnected")
      context.become(initialized(mode,server,listeners - listener,annotations))
    case InitDoc(id,content,mode) =>
      log.debug("client connected")
      context.watch(sender)
      context.become(initialized(mode,server,listeners + sender,annotations))
      if (server.revision > 0) {
        sender ! CombinedRemoteEdit(id, server.getCombinedHistory, server.revision)
        annotations.foreach { case (aid,as) =>
          sender ! RemoteAnnotations(id,aid,as)
        }
      }
    case Annotate(id,aid,as,rev) =>
      context.become(initialized(mode,server,listeners,annotations + (aid -> as)))
      (listeners - sender).foreach(_ ! RemoteAnnotations(id,aid,as))
    case Edit(id,op,rev) =>
      log.debug("applying edit")
      server.applyOperation(op,rev) match {
        case Success(op) =>
          context.become(initialized(mode,server,listeners,annotations.map {
            case (aid,as) => aid -> server.transformAnnotation(server.revision - 1, as).get
          }))
          sender ! AcknowledgeEdit(id)
          (listeners - sender).foreach(_ ! RemoteEdit(id,op))
        case Failure(e) =>
          log.error(e,"could not apply operation")
          sender ! ResetSnippet(id,server.text.mkString,server.revision)
      }
    case msg: SnippetMessage =>
      (listeners - sender).foreach(_ ! msg)
    case msg: RequestInfo =>
      (listeners - sender).foreach(_ ! msg)
    case msg: Information =>
      (listeners - sender).foreach(_ ! msg)
  }
}
