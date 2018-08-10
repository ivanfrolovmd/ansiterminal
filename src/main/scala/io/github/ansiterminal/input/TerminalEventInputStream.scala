package io.github.ansiterminal.input

import io.github.ansiterminal.input.TerminalEventInputStream._
import io.github.ansiterminal.input.TerminalRawInputStream._

import scala.util.{Success, Try}

class TerminalEventInputStream(val input: java.io.InputStream) extends InputStream[TerminalEvent] {
  private val terminalInput = new TerminalRawInputStream(input)

  override def read(): TerminalEvent            = convert(terminalInput.read())
  override def ready(): Boolean                 = terminalInput.ready()
  override def tryRead(): Option[TerminalEvent] = terminalInput.tryRead().map(convert)

  private def convert(token: Token): TerminalEvent = token match {
    case CharToken(8)           => Backspace
    case CharToken(9)           => Tab
    case CharToken(10)          => Enter
    case CharToken(13)          => Enter
    case CharToken(27)          => Escape
    case CharToken(127)         => Backspace // OSX specific?
    case CharToken(x) if x < 32 => Control(x)
    case CharToken(other)       => Character(other)
    case CSI(_, _, 'A')         => ArrowUp
    case CSI(_, _, 'B')         => ArrowDown
    case CSI(_, _, 'C')         => ArrowRight
    case CSI(_, _, 'D')         => ArrowLeft
    case CSI(_, _, 'H')         => Home
    case CSI(_, _, 'F')         => End
    case CSI(_, _, 'Z')         => ReverseTab
    case CSI("0", _, 'n')       => DeviceOk
    case CSI("3", _, 'n')       => DeviceNotOk
    case CSI("3", _, '~')       => Delete
    case CSI("5", _, '~')       => PageUp
    case CSI("6", _, '~')       => PageDown
    case SS3('P')               => F1
    case SS3('Q')               => F2
    case SS3('R')               => F3
    case SS3('S')               => F4
    case CSI("15", _, '~')      => F5
    case CSI("17", _, '~')      => F6
    case CSI("18", _, '~')      => F7
    case CSI("19", _, '~')      => F8
    case CSI("20", _, '~')      => F9
    case CSI("21", _, '~')      => F10
    case CSI("23", _, '~')      => F11
    case CSI("24", _, '~')      => F12
    case CSI("200", _, '~')     => StartBracket
    case CSI("201", _, '~')     => EndBracket
    case a @ CSI(params, _, 'R') =>
      params.split(';').map(x => Try(x.toInt)) match {
        case Array(Success(row), Success(column)) => CursorPosition(row, column)
        case _                                    => Unknown
      }
    case other => Unknown
  }
}

object TerminalEventInputStream {
  sealed trait TerminalEvent
  case class Character(char: Char) extends TerminalEvent
  case class Control(char: Char)   extends TerminalEvent
  case object Escape               extends TerminalEvent
  case object Backspace            extends TerminalEvent
  case object ArrowLeft            extends TerminalEvent
  case object ArrowRight           extends TerminalEvent
  case object ArrowUp              extends TerminalEvent
  case object ArrowDown            extends TerminalEvent
  case object Delete               extends TerminalEvent
  case object Home                 extends TerminalEvent
  case object End                  extends TerminalEvent
  case object PageUp               extends TerminalEvent
  case object PageDown             extends TerminalEvent
  case object Tab                  extends TerminalEvent
  case object ReverseTab           extends TerminalEvent
  case object Enter                extends TerminalEvent
  case object F1                   extends TerminalEvent
  case object F2                   extends TerminalEvent
  case object F3                   extends TerminalEvent
  case object F4                   extends TerminalEvent
  case object F5                   extends TerminalEvent
  case object F6                   extends TerminalEvent
  case object F7                   extends TerminalEvent
  case object F8                   extends TerminalEvent
  case object F9                   extends TerminalEvent
  case object F10                  extends TerminalEvent
  case object F11                  extends TerminalEvent
  case object F12                  extends TerminalEvent
  case object F13                  extends TerminalEvent
  case object F14                  extends TerminalEvent
  case object F15                  extends TerminalEvent
  case object F16                  extends TerminalEvent
  case object F17                  extends TerminalEvent
  case object F18                  extends TerminalEvent
  case object F19                  extends TerminalEvent
  case object StartBracket         extends TerminalEvent
  case object EndBracket           extends TerminalEvent
  case object DeviceOk             extends TerminalEvent
  case object DeviceNotOk          extends TerminalEvent
  case object Unknown              extends TerminalEvent
  //  case object Insert               extends TerminalEvent
//  case object MouseEvent           extends TerminalEvent
//  case object EOF                  extends TerminalEvent

  case class CursorPosition(row: Int, column: Int) extends TerminalEvent
}
