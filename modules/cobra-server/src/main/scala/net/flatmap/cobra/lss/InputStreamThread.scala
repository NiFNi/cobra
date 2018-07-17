package net.flatmap.cobra.lss

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.io.InputStream

import akka.actor.{Actor, ActorLogging, ActorRef}

/**
  * Created by nif on 08.05.18.
  */
class InputStreamThread(stream: InputStream, parent: ActorRef) extends Runnable {


  override def run() = {
    while (true) {
      val reader = new BufferedReader(new InputStreamReader(stream))
      var line: String = reader.readLine()
      var length: Int = -1
      while (length == -1) {
        if (line.startsWith("Content-Length: ")) {
          length = line.split(" ")(1).toInt
        } else {
          line = reader.readLine()
        }
      }
      //println(length)
      while (line != "") {
        line = reader.readLine()
      }
      val buffer = new Array[Char](length)

      val charsIn = reader.read(buffer, 0, length)

      val data = buffer.mkString
      parent ! Read(data)
    }
  }
}

