package io.github.ansiterminal.input

import java.util.concurrent.locks.ReentrantLock

import io.github.ansiterminal.Ansi

import scala.reflect.ClassTag

/**
  * Usage:
  * switchToControlMode
  * wipe Craps
  * send stuff (outside this class)
  * waitFor
  * switchBackToNormal
  */
class SwitchableInputStream[A](source: PushbackInputStream[A]) extends InputStream[A] {
  private val controlModeLock = new ReentrantLock()
  private val readingLock     = new ReentrantLock()

  // normal mode
  override def read(): A = {
    controlModeLock.lock() // check if is in control mode. wait until it's finished
    controlModeLock.unlock()
    val a = try {
      readingLock.lock()
      source.read()
    } finally readingLock.unlock()
    if (controlModeLock.tryLock()) {
      try a
      finally controlModeLock.unlock()
    } else {
      source.unread(a) // put back
      read()           // and wait until unlocked
    }
  }
  override def ready(): Boolean =
    if (controlModeLock.tryLock()) {
      try source.ready()
      finally controlModeLock.unlock()
    } else false

  override def tryRead(): Option[A] =
    if (controlModeLock.tryLock()) {
      try source.tryRead()
      finally controlModeLock.unlock()
    } else None

  // all methods below should happen in one thread
  // switches
  def switchToControlMode(): Unit = {
    controlModeLock.lock()
    Ansi().requestDeviceStatusReport().write() // TODO this is not generic (and A should not be generic then)
    readingLock.lock()
  }
  def switchToNormalMode(): Unit = {
    ensureThreadHoldsLock()
    controlModeLock.unlock()
    readingLock.unlock()
  }

  // control mode
  def wipe(p: A => Boolean): Unit = {
    ensureThreadHoldsLock()
    val available = readAllAvailable()
    source.unread(available.filterNot(p))
  }
  def wipe[B <: A]()(implicit classTag: ClassTag[B]): Unit = wipe(toPredicate(classTag))
  def waitFor(p: A => Boolean): A = {
    ensureThreadHoldsLock()
    var buf: List[A] = Nil
    while (true) {
      val a = source.read()
      if (p(a)) {
        source.unread(buf)
        return a
      } else {
        buf = a :: buf
      }
    }
    throw new IllegalStateException("unreachable") // TODO this is ugly
  }
  def waitFor[B <: A]()(implicit classTag: ClassTag[B]): B = waitFor(toPredicate(classTag)).asInstanceOf[B]

  // utils
  private def toPredicate[X, Y <: X](classTag: ClassTag[Y]): X => Boolean = x => classTag.runtimeClass.isInstance(x)
  private def ensureThreadHoldsLock(): Unit = {
    if (!controlModeLock.isHeldByCurrentThread) {
      throw new IllegalMonitorStateException(
        "All control methods should be executed in the same method that caused control switch")
    }
  }
  private def readAllAvailable(): List[A] =
    Stream.continually(source.tryRead()).takeWhile(_.isDefined).map(_.get).toList
}
