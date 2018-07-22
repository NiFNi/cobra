package net.flatmap.cobra.lss
import java.nio.file.{Files, FileSystems, Paths}
import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorLogging, Props}

import scala.util.Try

/**
  * Created by nif on 02.03.18.
  */

object FileSaver {
  def props(workspacePath: String) = Props(new FileSaver(workspacePath))
}


class FileSaver(workspacePath: String) extends Actor with ActorLogging {
  log.info("filesaver started")

  override def receive: Receive = {
    case (str: String, path: String) => {
      log.info("file saved")
      val uri = "file://" + workspacePath + path
      save(str, workspacePath + path)
      context.parent ! DidSaveTextDocumentParams(TextDocumentIdentifier(uri))
    }
    case _ => log.warning("Filesaver did get something that were not strings")
  }

  def save(content: String, path: String): Try[Unit] = {
    Try(Files.write(Paths.get(path), content.getBytes(StandardCharsets.UTF_8)))
  }
}
