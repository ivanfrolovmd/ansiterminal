package io.github.ansiterminal.input

import Stuff._

trait InputStream[+A] {
  def read(): A
  def ready(): Boolean
  def tryRead(): Option[A]
}

object Stuff {
  type OutputStream = java.io.OutputStream
}

abstract class SwitchableStream[+A] {
  def normalIn: InputStream[A]
  def normalOut: OutputStream

  def controlIn: InputStream[A]
  def controlOut: OutputStream

  /*
  Locking should be made on the InputStream? - how to make sure no one accesses streams during the ops?
   */
  def inControl[B](code: (InputStream[A], OutputStream) => B): B
}


