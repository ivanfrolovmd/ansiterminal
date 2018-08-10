package io.github.ansiterminal.input

import java.util.concurrent.locks.ReentrantLock

import scala.annotation.tailrec

class SwitchableStreams(val inOriginal: java.io.InputStream, val outOriginal: java.io.OutputStream) {
  private val readingLock = new ReentrantLock()
  private val controlLock = new ReentrantLock()

  private var readByte: Option[Int] = None

  def inControlMode[A](code: (java.io.OutputStream, java.io.InputStream) => A): A = {
    controlLock.lock()
    try code(controlOut, controlIn)
    finally controlLock.unlock()
  }

  def in(): java.io.InputStream = new java.io.InputStream {
    @tailrec
    override def read(): Int = {
      readingLock.lock()
      try {
        val b = inOriginal.read()
        if (controlLock.tryLock()) {
          controlLock.unlock()
          return b
        } else {
          readByte = Some(b)
        }
      } finally readingLock.unlock()

      try controlLock.lock() // wait until control lock is released
      finally controlLock.unlock()
      read()
    }

    override def available(): Int = {
      if (readingLock.tryLock()) {
        try inOriginal.available()
        finally readingLock.unlock()
      } else 0
    }

    override def close(): Unit = inOriginal.close()
  }

  def out(): java.io.OutputStream = new java.io.OutputStream {
    override def write(b: Int): Unit = {
      controlLock.lock()
      try outOriginal.write(b)
      finally controlLock.unlock()
    }
    override def flush(): Unit = {
      controlLock.lock()
      try outOriginal.flush()
      finally controlLock.unlock()
    }
    override def close(): Unit = outOriginal.close()
  }

  private val controlIn: java.io.InputStream = () => {
    readingLock.lock()
    try {
      readByte match {
        case Some(b) => readByte = None; b
        case None    => inOriginal.read()
      }
    } finally readingLock.unlock()
  }
  private val controlOut: java.io.OutputStream = (b: Int) => outOriginal.write(b)
}
