package io.github.ansiterminal

import org.scalatest.{FreeSpec, Matchers}

class AnsiTest extends FreeSpec with Matchers {
  "Ansi" - {
    "should display correct printable length" in {
      val ansi = Ansi()
        .bold()
        .a("test")
        .boldOff()
        .newLine()
        .fg(Ansi.Color.Red)
        .a(1234)
        .resetAllAttributes()
        .up()
        .down()
        .eraseScreen(Ansi.ScreenEraseKind.WholeScreen)

      ansi.toString.length shouldBe 39
      ansi.printableLength shouldBe 9
    }

    "should merge ansi strings correctly" in {
      val first = Ansi()
        .bold()
        .a("test")
        .boldOff()

      val second = Ansi().newLine()

      val third = Ansi()
        .fg(Ansi.Color.Red)
        .a(1234)
        .resetAllAttributes()

      val expected = Ansi()
        .bold()
        .a("test")
        .boldOff()
        .newLine()
        .fg(Ansi.Color.Red)
        .a(1234)
        .resetAllAttributes()

      val actual = first ++ second ++ third

      actual.toString shouldBe expected.toString
      actual.printableLength shouldBe expected.printableLength
      actual shouldBe expected
    }

    "should ignore instructions in conditional block" in {
      val ansi = Ansi()
        .when(condition = true) { _.a("this should go in") }
        .when(condition = false) { _.a("this should be ignored") }

      ansi.toString shouldBe "this should go in"
    }
  }
}
