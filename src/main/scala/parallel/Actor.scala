package parallel

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{Callable,ExecutorService}
import annotation.tailrec

final class Actor[A](executor: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw(_)):
  self =>

  private val tail = new AtomicReference(new Node[A]())
  private val suspended = new AtomicInteger(1)
  private val head = new AtomicReference(tail.get)

  infix def !(a: A): Unit =
    val n = new Node(a)
    head.getAndSet(n).lazySet(n)
    trySchedule()

  private def trySchedule(): Unit =
    if suspended.compareAndSet(1, 0) then schedule()

  private def schedule(): Unit =
    executor.submit(new Callable[Unit] { def call: Unit = act() })
    ()

  private def act(): Unit =
    val t = tail.get
    val n = batchHandle(t, 1024)
    if n ne t then
      n.a = null.asInstanceOf[A]
      tail.lazySet(n)
      schedule()
    else
      suspended.set(1)
      if n.get ne null then trySchedule()

  @tailrec
  private def batchHandle(t: Node[A], i: Int): Node[A] =
    val n = t.get
    if n ne null then
      try
        handler(n.a)
      catch
        case ex: Throwable => onError(ex)
      if i > 0 then batchHandle(n, i - 1) else n
    else t

private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]
