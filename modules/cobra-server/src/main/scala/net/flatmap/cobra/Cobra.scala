package net.flatmap.cobra

import better.files.File.OpenOptions
import net.flatmap.cobra.isabelle.IsabelleUtil
import better.files._
import net.flatmap.cobra.lss._
import akka.actor._
import akka.stream.ActorMaterializer
import play.api.libs.json._

/**
  * Created by martin on 03.02.16.
  */
object Cobra extends App {
  val version = "1.0.5"

  private def assume(cond: Boolean, msg: String) = if (!cond) {
    println("failed to initialize: " + msg)
    sys.exit()
  }

  private def printLogo() = {
    println( """        ____      _                     """)
    println( """       / ___|___ | |__  _ __ __ _ TM    """)
    println( """      | |   / _ \| '_ \| '__/ _` |      """)
    println( """      | |__| (_) | |_) | | | (_| |      """)
    println( """       \____\___/|_.__/|_|  \__,_|      """)
    println( """________________________________________""")
    println(s"""| version $version - (c) 2016 Martin Ring |""")
    println()
  }

  if (args.length > 1 && args.head == "configure") {
    args.tail.foreach {
      case "isabelle" =>
        println("# Configuring Isabelle")
        sys.env.get("ISABELLE_HOME").fold {
          IsabelleUtil.locateInstallation.fold {
            IsabelleUtil.locateOldInstallation.fold {
              println("No prior isabelle installation was detected.")
            } { old =>
              println(s"An incompatible Isabelle installation was found at $old.")
            }
            IsabelleUtil.download()
          } { path =>
            println(s"A compatible Isabelle distribution was found at '$path'.")
            println("Nothing to do.")
          }
        } { isa_home =>
          println(s"Isabelle 2016 is already configured (the ISABELLE_HOME environment variable points to $isa_home)")
        }
      case "haskell" =>
        import sys.process._
        println("# Configuring Haskell")
        val r = "^ghc-mod version ([^\\s]+) compiled by GHC ([^\\s]+)$".r
        try {
          val List(m,g) = r.findFirstMatchIn(Seq("ghc-mod", "version").lineStream.head).get.subgroups
          println(s"Found interface for GHC $g")
          println("Nothing to do.")
        } catch {
          case io: java.io.IOException =>
            println("ghc-mod does not exist on your PATH")
          case m: NoSuchElementException =>
            println("ERROR: ghc-mod is available on your PATH but doesn't seem to be compatible.")
            println("Please remove ghc-mod and run 'cobra configure haskell again'")
        }
      case "scala" =>
        println("# Configuring Scala")
        println("Scala language support is already enabled")
        println("Nothing to do.")
      case other =>
        println(s"sorry, language support for '$other' is currently not available")
        sys.exit(-1)
    }
    sys.exit()
  }
  else if ((args.length == 1 || args.length == 2) && args.head == "new") {
    printLogo()
    val name = if (args.length > 1) args(1) else {
      scala.io.StdIn.readLine("please enter a name for the new presentation: ")
    }
    println(s"creating new cobra presentation '$name'...")
    val dir = File(name)
    assume(!dir.exists() || !dir.isDirectory, s"directory '$name' exists already")
    assume(dir.createDirectories().exists, "could not create directory")
    val isa_home = sys.env.get("ISABELLE_HOME").fold {
      println()
      IsabelleUtil.locateInstallation.fold {
        IsabelleUtil.locateOldInstallation.fold {
          println("WARN: No Isabelle installation was detected.")
        } { old =>
          println(s"WARN: An incompatible Isabelle installation was found ($old)")
        }
        println(" (i) if you want to use isabelle, please run 'cobra configure isabelle'")
        println("     or configure env.isabelle_home in cobra.conf")
        println()
        "# configure if you want to use isabelle\n" +
        "# ISABELLE_HOME = '...'"
      } { case path =>
        println(s"A compatible Isabelle distribution was found at $path")
        s"# ISABELLE_HOME = '$path'"
      }
    } { isa_home =>
      s"# isabelle_home = $isa_home"
    }

    val conf = scala.io.Source.fromURL(getClass.getResource("/template-cobra.conf")).mkString
      .replaceAll("\\{\\s*title\\s*\\}", s""""$name"""")
      .replaceAll("\\{\\s*lang\\s*\\}", '"' + System.getProperty("user.language") + '"')
      .replaceAll("\\{\\s*isa_home\\s*\\}", s"""$isa_home""")
    
    val slides = scala.io.Source.fromURL(getClass.getResource("/template-slides.html")).mkString
      .replaceAll("\\{\\s*title\\s*\\}", name)

    (dir / "cobra.conf").createIfNotExists() < (conf)
    (dir / "slides.html").createIfNotExists() < (slides)

    println("the presentation has been successfully initialized.")
    println(s"you may start presentation with 'cobra $name'")

    sys.exit()
  }

  if (args.length > 1) {
    println(s"invalid arguments: '${args.mkString(" ")}'")
    sys.exit(-1)
  }

  val directory = if (args.isEmpty) File(".") else File(args.head)

  { // initialize
    printLogo()
    assume(directory.exists, "could not find " + directory)
    assume(directory.isDirectory, directory + " is not a directory")
    assume(directory.isReadable, "can not read " + directory)
    assume((directory / "slides.html").exists(), "no slides.html found")
    assume((directory / "cobra.conf").exists(), "no cobra.conf found")

    val server = new CobraServer(directory)
    server.start()
    while (scala.io.StdIn.readLine != "exit") ()
    server.stop()
  }
}

object Test extends App {

  main()

  def main() {
    implicit val system = ActorSystem("abc")
    implicit val materializer = ActorMaterializer()
    var act = system.actorOf(LsCommunicator.props)
    val workspacePath = "/tmp/test"
    act ! InitializeParams(0, workspacePath, ClientCapabilities())
    Thread.sleep(2000)
    act ! Initialized()
    Thread.sleep(2000)
    act ! DidOpenTextDocumentParams(TextDocumentItem("file:///tmp/test/test.dart", "dart", 0, """void main() {
  var name = 'Voyager I';
  print(name);
}"""))
    Thread.sleep(2000)
    val a: TextDocumentDefinitionRequest = TextDocumentDefinitionRequest(TextDocumentPositionParams(TextDocumentIdentifier("file:///tmp/test/test.dart"), Position(2, 10)))
//    implicit val b = new Format[TextDocumentDefinitionRequest] {
//      override def reads(json: JsValue) = Reads.of[TextDocumentPositionParams].reads(json).map(TextDocumentDefinitionRequest.apply)
//      override def writes(o: TextDocumentDefinitionRequest) = TextDocumentDefinitionRequest.unapply(o) match {
//        case a: Option[TextDocumentPositionParams] => Writes.of[TextDocumentPositionParams].writes(a.get)
//      }
//    }
    act ! TextDocumentDefinitionRequest(TextDocumentPositionParams(TextDocumentIdentifier("file:///tmp/test/test.dart"), Position(2, 10)))
    var con = true
    while (con) {
      val in = scala.io.StdIn.readLine
      println(in)
      in match {
        case "exit" => con = false
        case "" =>
          println("killing old actor")
          act ! PoisonPill
          println("restarting new")
          act = system.actorOf(LsCommunicator.props)
          val workspacePath = "/tmp/test"
          act ! InitializeParams(0, workspacePath, ClientCapabilities())
          Thread.sleep(2000)
          act ! Initialized()
      }
    }
    println("cleaning up")
    println("shutdown language server")
    act ! Shutdown()
    act ! PoisonPill
    println("poisoned actor")
    system.terminate()
    println("terminated system")
  }
}
