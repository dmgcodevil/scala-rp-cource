package com.dmgcodevil.recap.generator.future

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit._
/**
 * Created by dmgcodevil on 5/2/2015.
 */
object TestApp {

  def test(): Unit = {
    val f1 = Future {
      "res1"
    }
    val f2 = Future {
      "res2"
    }

    def compute(r1: String, r2: String) = r1 + " " + r2

    val result = for {
      r1 <- f1
      r2 <- f2
    } yield compute(r1, r2)

    val result2 = f1.flatMap(r1 => f2.map(r2 => compute(r1, r2)))

    result onSuccess {
      case r => println(r)
    }
    result2 onSuccess {
      case r => println(r)
    }


  }

  def all[T](fs: List[Future[T]]): Future[List[T]] = {
    fs match {
      case Nil => Future(Nil)
      case (ft :: fs) =>
        for {
          t <- ft
          ts <- all(fs)
        } yield t :: ts
    }
  }

  def any[T](fs: List[Future[T]]): Future[T] = {
    val p = Promise[T]()
    fs.foreach(f => f onComplete (t => p.tryComplete(t)))
    p.future
  }

  def delay(t: Duration): Future[Unit] = {
    Future(Await.ready(Promise().future, t))
  }

  def main(args: Array[String]) {
    TestApp.delay(Duration(10, "millis")) onComplete {t=> println("done") }
    Thread.sleep(1000)
  }
}
