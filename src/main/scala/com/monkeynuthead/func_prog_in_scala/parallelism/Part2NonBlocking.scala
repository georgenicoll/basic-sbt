package com.monkeynuthead.func_prog_in_scala.parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, Callable, ExecutorService, TimeUnit}

object Part2NonBlocking {

  sealed trait Future[A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(k: (A) => Unit): Unit =
          k(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(k: (A) => Unit): Unit =
          eval(es)(a(es)(k))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        override def call(): Unit = r
      })

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A,B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

//    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
//      es => {
//        UnitFuture(as.filter(f))
//      }
//    }

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(k: (C) => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A,B]](es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(k(f(a,b)))
            }

            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(k(f(a,b)))
            }
          }

          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    def map[A, B](a: Par[A])(f: A => B): Par[B] =
      map2(a, unit(()))((a, _) => f(a))

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
      map2(a, map2(b, c)((b, c) => (b, c))) { (a, bc) =>
        val (b, c) = bc
        f(a, b, c)
      }

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
      map2(map2(a, b)((_, _)), map2(c, d)((_, _))) { (ab, cd) =>
        val (a, b) = ab
        val (c, d) = cd
        f(a, b, c, d)
      }

    def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
      map2(map2(a, b)((_, _)), map3(c, d, e)((_, _, _))) { (ab, cde) =>
        val (a, b) = ab
        val (c, d, e) = cde
        f(a, b, c, d, e)
      }

    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown() }
      latch.await
      ref.get
    }

//    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

//    def parDivideAndConquer[A, B](values: IndexedSeq[A])(convert: A => B)(zero: B)(combine: (B, B) => B): Par[B] =
//      if (values.size <= 1)
//        Par.unit(values.headOption.map(convert) getOrElse zero)
//      else {
//        val (l, r) = values.splitAt(values.length / 2)
//        Par.map2(Par.fork(parDivideAndConquer(l)(convert)(zero)(combine)),
//          Par.fork(parDivideAndConquer(l)(convert)(zero)(combine)))(combine)
//      }

//    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
//      p(e).get == p2(e).get

  }

  import Par._

//  def sum(ints: IndexedSeq[Int]): Par[Int] = parDivideAndConquer(ints)(identity)(0)(_ + _)

//  def totalWords(paras: IndexedSeq[String]): Par[Int] =
//    parDivideAndConquer(paras)(_.split(" ").length)(0)(_ + _)

}
