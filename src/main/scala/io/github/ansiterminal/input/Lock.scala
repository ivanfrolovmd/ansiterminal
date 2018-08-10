package io.github.ansiterminal.input

import java.util.concurrent.locks.ReentrantLock

class Lock {
  private val lock = new ReentrantLock()

  def blocking[A](code: => A): A = {
    lock.lock()
    try code
    finally lock.unlock()
  }

  def tryToLock[A](ifLocked: => A)(ifFree: => A): A = {
    val lockAcquired = lock.tryLock()
    if (lockAcquired) {
      try ifFree
      finally lock.unlock()
    } else {
      ifLocked
    }
  }

  def isHeldByCurrentThread: Boolean = lock.isHeldByCurrentThread
}
