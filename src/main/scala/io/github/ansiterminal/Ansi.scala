package io.github.ansiterminal

import java.io.OutputStream
import java.nio.charset.StandardCharsets

import io.github.ansiterminal.Ansi.LineEraseKind.ToEnd
import io.github.ansiterminal.Ansi._

import scala.util.matching.Regex

final class Ansi private (private val buf: Vector[Char] = Vector.empty) {

  def ++(other: Ansi): Ansi = new Ansi(this.buf ++ other.buf)

  def a(block: Ansi => Ansi): Ansi = when(condition = true)(block)
  def a(other: Ansi): Ansi         = this ++ other
  def a(value: => Any): Ansi       = append(value.toString)

  def newLine(n: Int = 1): Ansi = append("".padTo(n, '\n'))

  def when(condition: Boolean)(block: Ansi => Ansi): Ansi = if (condition) block(this) else this

  def up(delta: Int = 1): Ansi                 = if (delta > 0) appendCsi('A', delta.toString) else appendNoop()
  def down(delta: Int = 1): Ansi               = if (delta > 0) appendCsi('B', delta.toString) else appendNoop()
  def right(delta: Int = 1): Ansi              = if (delta > 0) appendCsi('C', delta.toString) else appendNoop()
  def left(delta: Int = 1): Ansi               = if (delta > 0) appendCsi('D', delta.toString) else appendNoop()
  def moveToNextLine(delta: Int = 1): Ansi     = if (delta > 0) appendCsi('E', delta.toString) else appendNoop()
  def moveToPreviousLine(delta: Int = 1): Ansi = if (delta > 0) appendCsi('F', delta.toString) else appendNoop()
  def moveToRelativeLine(delta: Int = 0): Ansi = delta match {
    case x if x < 0 => moveToPreviousLine(-x)
    case 0          => moveToColumn(0)
    case x if x > 0 => moveToNextLine(x)
  }
  def moveToColumn(column: Int): Ansi = if (column >= 0) appendCsi('G', (column + 1).toString) else appendNoop()
  def moveTo(row: Int, column: Int): Ansi =
    if (row >= 0 && column >= 0) appendCsi('H', s"${row + 1};${column + 1}") else appendNoop()
  def moveTo(position: Product2[Int, Int]): Ansi   = moveTo(position._1, position._2)
  def eraseScreen(kind: ScreenEraseKind): Ansi     = appendCsi('J', kind.v)
  def eraseLine(kind: LineEraseKind = ToEnd): Ansi = appendCsi('K', kind.v)
  def scrollUp(rows: Int): Ansi                    = if (rows > 0) appendCsi('S', rows.toString) else appendNoop()
  def scrollDown(rows: Int): Ansi                  = if (rows > 0) appendCsi('T', rows.toString) else appendNoop()

  def saveCursorPosition(): Ansi    = appendCsi('s')
  def restoreCursorPosition(): Ansi = appendCsi('u')
  def requestCursorPosition(): Ansi = appendCsi('n', "6")

  def requestDeviceStatusReport(): Ansi = appendCsi('n', "5")

  def fg(color: Color, bright: Boolean = false): Ansi =
    appendCsi('m', ((if (bright) 60 else 0) + 30 + color.ix).toString)
  def bg(color: Color, bright: Boolean = false): Ansi =
    appendCsi('m', ((if (bright) 60 else 0) + 40 + color.ix).toString)
  def colors(fg: Color, fgBright: Boolean, bg: Color, bgBright: Boolean): Ansi =
    appendCsi('m', s"${(if (fgBright) 60 else 0) + 30 + fg.ix};${(if (bgBright) 60 else 0) + 40 + bg.ix}")
  def bold(): Ansi               = appendCsi('m', "1")
  def boldOff(): Ansi            = appendCsi('m', "22")
  def italic(): Ansi             = appendCsi('m', "3")
  def italicOff(): Ansi          = appendCsi('m', "23")
  def underline(): Ansi          = appendCsi('m', "4")
  def underlineOff(): Ansi       = appendCsi('m', "24")
  def blink(): Ansi              = appendCsi('m', "5")
  def blinkOff(): Ansi           = appendCsi('m', "25")
  def inverse(): Ansi            = appendCsi('m', "7")
  def inverseOff(): Ansi         = appendCsi('m', "27")
  def resetAllAttributes(): Ansi = appendCsi('m', "0")

  def showCursor(): Ansi       = privateOn(25)
  def hideCursor(): Ansi       = privateOff(25)
  def bracketedModeOn(): Ansi  = privateOn(2004)
  def bracketedModeOff(): Ansi = privateOff(2004)

  override lazy val toString: String = buf.mkString
  lazy val printableLength: Int      = CsiRegex.replaceAllIn(toString, "").length
  def write(out: OutputStream = System.out): Unit = {
    out.write(toString.getBytes(StandardCharsets.UTF_8))
    out.flush()
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Ansi => this.buf.equals(other.buf)
    case _           => false
  }
  override def hashCode(): Int = buf.hashCode()

  private def append(a: => String): Ansi = new Ansi(buf ++ a)
  private def appendCsi(finalChar: Char, params: String = "", intermediate: String = ""): Ansi =
    new Ansi(buf ++ EscSeq ++ params ++ intermediate :+ finalChar)
  private def appendNoop(): Ansi = this
  private def privateOn(n: Int)  = appendCsi('h', s"?$n")
  private def privateOff(n: Int) = appendCsi('l', s"?$n")
}

object Ansi {
  def apply(): Ansi             = new Ansi()
  def apply(initial: Any): Ansi = new Ansi().a(initial)

  def merge(l: Ansi, r: Ansi): Ansi = l ++ r

  private val ESC: Char       = 27.toChar
  private val EscSeq: String  = s"$ESC["
  private val CsiRegex: Regex = "\\x1B\\[[\\x30-\\x3F]*[\\x20-\\x2F]*[\\x40-\\x7E]".r

  sealed trait Color { val ix: Int }
  object Color {
    case object Black   extends Color { override val ix: Int = 0 }
    case object Red     extends Color { override val ix: Int = 1 }
    case object Green   extends Color { override val ix: Int = 2 }
    case object Yellow  extends Color { override val ix: Int = 3 }
    case object Blue    extends Color { override val ix: Int = 4 }
    case object Magenta extends Color { override val ix: Int = 5 }
    case object Cyan    extends Color { override val ix: Int = 6 }
    case object White   extends Color { override val ix: Int = 7 }
    case object Default extends Color { override val ix: Int = 9 }
  }

  sealed trait ScreenEraseKind { val v: String }
  object ScreenEraseKind {
    case object ToBottomRight               extends ScreenEraseKind { override val v: String = "0" }
    case object ToTopLeft                   extends ScreenEraseKind { override val v: String = "1" }
    case object WholeScreen                 extends ScreenEraseKind { override val v: String = "2" }
    case object WholeScreenWithScrollBuffer extends ScreenEraseKind { override val v: String = "3" }
  }

  sealed trait LineEraseKind { val v: String }
  object LineEraseKind {
    case object ToEnd     extends LineEraseKind { override val v: String = "0" }
    case object ToStart   extends LineEraseKind { override val v: String = "1" }
    case object WholeLine extends LineEraseKind { override val v: String = "2" }
  }
}
