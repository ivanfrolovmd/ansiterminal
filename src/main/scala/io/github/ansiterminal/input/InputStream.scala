package io.github.ansiterminal.input

trait InputStream[+A] {
  def read(): A
  def ready(): Boolean
  def tryRead(): Option[A]
}
