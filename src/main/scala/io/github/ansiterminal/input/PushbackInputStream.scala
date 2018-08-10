package io.github.ansiterminal.input

import java.util.concurrent.locks.ReentrantLock

class PushbackInputStream[A](source: InputStream[A]) extends InputStream[A] {
  private var buffer: List[A] = Nil
  private val lock            = new ReentrantLock()

  def read(): A =
//    lock
//      .blocking {
//        buffer match {
//          case head :: tail => buffer = tail; Some(head)
//          case Nil          => None
//        }
//      }
//      .getOrElse(source.read())
//
//
  {
    lock.lock()
    buffer match {
      case head :: tail => buffer = tail; lock.unlock(); head
      case Nil          => lock.unlock(); source.read()
    }
  }
  def ready(): Boolean = {
    lock.lock()
    try {
      buffer.nonEmpty || source.ready()
    } finally lock.unlock()
  }
  def tryRead(): Option[A] = {
    if (lock.tryLock()) {
      try {
        val result = buffer.headOption.orElse(source.tryRead())
        if (buffer.nonEmpty) { buffer = buffer.tail }
        result
      } finally lock.unlock()
    } else {
      None
    }
  }
  def unread(a: A): Unit = {
    lock.lock()
    buffer = a :: buffer
    lock.unlock()
  }
  def unread(a: Seq[A]): Unit = {
    lock.lock()
    buffer = a.foldLeft(buffer)((l, a) => a :: l)
    lock.unlock()
  }
}
