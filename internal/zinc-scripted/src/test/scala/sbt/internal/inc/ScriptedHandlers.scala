package sbt.internal.inc

import java.io.File

import sbt.internal.scripted._
import sbt.internal.util.ManagedLogger

class SleepingHandler(val handler: StatementHandler, delay: Long) extends StatementHandler {
  type State = handler.State
  override def initialState: State = handler.initialState
  override def apply(command: String, arguments: List[String], state: State): State = {
    val result = handler.apply(command, arguments, state)
    Thread.sleep(delay)
    result
  }
  override def finish(state: State) = handler.finish(state)
}

class IncScriptedHandlers(globalCacheDir: File) extends HandlersProvider {
  def getHandlers(config: ScriptConfig): Map[Char, StatementHandler] = Map(
    '$' -> new SleepingHandler(new ZincFileCommands(config.testDirectory()), 500),
    '#' -> new sbt.internal.scripted.BasicStatementHandler {
      def apply(command: String, args: List[String]) = {
        println(s">#>#>#>#>#> Comment $command ${args.mkString(" ")}")
      }
    },
    '>' -> {
      val logger: ManagedLogger =
        config.logger() match {
          case x: ManagedLogger => x
          case _                => sys.error("Expected ManagedLogger")
        }
      new IncHandler(config.testDirectory(), globalCacheDir, logger)
    }
  )
}
