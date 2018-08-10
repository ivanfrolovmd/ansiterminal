package io.github.ansiterminal

import java.io.OutputStream

import io.github.ansiterminal.Terminal._
import io.github.ansiterminal.input.TerminalEventInputStream.TerminalEvent
import io.github.ansiterminal.input._

import scala.Function.const

class Terminal(val input: java.io.InputStream = System.in, val out: OutputStream = System.out) {
  private val in: SwitchableInputStream[TerminalEvent] = new SwitchableInputStream(new PushbackInputStream(new TerminalEventInputStream(input)))
  private var resizeListener: TerminalSize => Unit = const(Unit)

  // reading
  def read(): TerminalEvent = in.read()

  // writing
  def print(string: String): Unit    = ???
  def putCharacter(char: Char): Unit = out.write(char)
  def printAt(string: String, row: Int, column: Int): Unit =
    Ansi()
      .saveCursorPosition()
      .moveTo(row, column)
      .a(string)
      .restoreCursorPosition()
      .write()

  //TODO remove?
  def flush(): Unit = out.flush()

  // controls
  def setCursorPosition(pos: CursorPosition): Unit = Ansi().moveTo(pos.row, pos.column).write()

  // TODO generalize controls

  private var terminalSize: Option[TerminalSize] = None // TODO implement correctly
  def getTerminalSize: TerminalSize = {
    if (terminalSize.isDefined) return terminalSize.get
    in.switchToControlMode()
    try {
      in.wipe[TerminalEventInputStream.CursorPosition]

      Ansi()
        .saveCursorPosition()
        .moveTo(5000, 5000)
        .requestCursorPosition()
        .restoreCursorPosition()
        .write()

      val position = in.waitFor[TerminalEventInputStream.CursorPosition]

      terminalSize = Some(TerminalSize(position.row, position.column))
      terminalSize.get
    } finally in.switchToNormalMode()
  }
  def getCursorPosition: CursorPosition = {
    in.switchToControlMode()
    try {
      in.wipe[TerminalEventInputStream.CursorPosition]

      Ansi()
        .requestCursorPosition()
        .write()

      val pos = in.waitFor[TerminalEventInputStream.CursorPosition]
      CursorPosition(pos.row, pos.column)
    } finally in.switchToNormalMode()
  }

  def registerResizeListener(listener: TerminalSize => Unit): Unit = {
    resizeListener = listener
    // todo implement events (listen on WINCH signal); update internal knowledge of terminalSize
  }
}

object Terminal {
  case class CursorPosition(row: Int, column: Int) extends Product2[Int, Int] {
    def withColumn(newValue: Int): CursorPosition   = copy(column = newValue)
    def withRelativeRow(delta: Int): CursorPosition = copy(row = row + delta)
    def withRelative(deltaRow: Int, deltaCol: Int): CursorPosition =
      copy(row = row + deltaRow, column = column + deltaCol)
    override def _1: Int = row
    override def _2: Int = column
  }
  case class TerminalSize(rows: Int, columns: Int)
}
