package io.github.ansiterminal.input

import java.io.{CharArrayReader, InputStreamReader, PushbackReader}
import java.nio.charset.StandardCharsets

import io.github.ansiterminal.input.TerminalRawInputStream._

import scala.collection.mutable

class TerminalRawInputStream(input: java.io.InputStream) extends InputStream[Token] {
  private val MaxSeqSize = 256
  private val in         = new PushbackReader(new InputStreamReader(input, StandardCharsets.UTF_8), MaxSeqSize)
  private val ESC        = 27.toChar
  private val buffer     = new Array[Char](MaxSeqSize)
  private val lock       = new Lock()

  // blocking
  def read(): Token = lock.blocking {
    val char = in.read().toChar
    if (char != ESC) {
      CharToken(char)
    } else {
      in.unread(char)
      val totalBytesRead = readIntoBuffer(buffer, in, MaxSeqSize)
      implicit val bufferredReader: PushbackReader =
        new PushbackReader(new CharArrayReader(buffer, 0, totalBytesRead), 1)

      val escapeSeq = for {
        esc    <- readChar(ESC)
        second <- readChar(0x40 to 0x5F)
        result <- second match {
          case '[' => readCsi()
          case 'N' => for (char <- doTryRead()) yield SS2(char)
          case 'O' => for (char <- doTryRead()) yield SS3(char)
          case _   => Some(UnknownEscapeSeq)
        }
      } yield result

      escapeSeq.fold[Token] {
        in.unread(buffer, 0, totalBytesRead)
        CharToken(in.read().toChar)
      } { x =>
        Stream.continually(bufferredReader.read()).takeWhile(_ != -1).reverse.foreach(in.unread) // push back the rest
        x
      }
    }
  }

  // non-blocking
  def ready(): Boolean         = lock.tryToLock(false)(in.ready())
  def tryRead(): Option[Token] = lock.tryToLock(Option.empty[Token])(Some(read()))

  /*
  The ESC [ is followed by any number (including none) of "parameter bytes" in the range 0x30–0x3F (ASCII 0–9:;<=>?),
  then by any number of "intermediate bytes" in the range 0x20–0x2F (ASCII space and !"#$%&'()*+,-./), then finally by
  a single "final byte" in the range 0x40–0x7E (ASCII @A–Z[\]^_`a–z{|}~)
   */
  private def readCsi()(implicit ins: PushbackReader): Option[CSI] = {
    val params       = readArray(0x30 to 0x3F)
    val intermediate = readArray(0x20 to 0x2F)
    for (finalChar <- readChar(0x40 to 0x7E)) yield CSI(params, intermediate, finalChar)
  }

  private def doTryRead()(implicit reader: PushbackReader): Option[Char] =
    if (reader.ready()) Some(reader.read().toChar) else None
  private def readChar(char: Char)(implicit reader: PushbackReader): Option[Char] =
    doTryRead().flatMap(c => if (char == c) Some(c) else None)
  private def readChar(range: Range.Inclusive)(implicit reader: PushbackReader): Option[Char] =
    doTryRead().flatMap(c => if (range.contains(c)) Some(c) else None)
  private def readArray(range: Range.Inclusive)(implicit ins: PushbackReader): String = {
    val array = mutable.ArrayBuilder.make[Char]
    var stop  = false
    while (!stop && ins.ready()) {
      val byte = ins.read()
      if (range.contains(byte)) {
        array += byte.toChar
      } else {
        ins.unread(byte)
        stop = true
      }
    }
    new String(array.result())
  }
  private def readIntoBuffer(buff: Array[Char], ins: PushbackReader, length: Int): Int = {
    var ix = 0
    while (ins.ready()) {
      ins.read(buff, ix, 1)
      ix += 1
    }
    ix
  }
}

object TerminalRawInputStream {
  sealed trait Token
  case class CharToken(char: Char)                                      extends Token
  case class CSI(params: String, intermediate: String, finalChar: Char) extends Token
  case class SS2(char: Char)                                            extends Token
  case class SS3(char: Char)                                            extends Token
  case object UnknownEscapeSeq                                          extends Token
}
