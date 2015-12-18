package com.monkeynuthead.func_prog_in_scala.props

import scala.language.implicitConversions
import scala.language.postfixOps

class IntPimp(i: Int) {

  def +%(of: Int) = {
    val mod = i % of
    if (mod < 0) mod + of else mod
  }

}

object IntPimp {

  implicit def intPimp(i: Int): IntPimp = new IntPimp(i)

}

import Prop._

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case object Proved extends Result {
  def isFalsified = false
}
case class Falsified(failures: FailureReasons, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {

  def &&(p: Prop): Prop = Prop { (max, cases, rng) =>
    run(max, cases, rng) match {
      case Passed | Proved => p.run(max, cases, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop { (max, cases, rng) =>
    run(max, cases, rng) match {
      case Falsified(f, s) => p.run(max, cases, rng)
      case x => x
    }
  }

}

object Prop {

  type SuccessCount = Int
  type FailureReasons = List[String]
  type TestCases = Int
  type MaxSize = Int

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Failsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified(List("()"), 0)
  }

}

case class Gen[+A](sample: State[RNG,A]) {

  def run = sample.run(RNG.Simple(System.currentTimeMillis()))._1

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap { a =>
    val genB = f(a)
    genB.sample
  })

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap ( n => Gen(State.sequence(List.fill(n)(sample))) )

  def unsized: SGen[A] = SGen((forSize) => this)

}

case class SGen[+A](forSize: Int => Gen[A]) {

  def map[B](f: A => B): SGen[B] = SGen { i => forSize(i).map(f) }

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen { i => forSize(i).flatMap(f) }

}

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def option[A](g: Gen[A]): Gen[Option[A]] = Gen(State(RNG.boolean).flatMap { b =>
    if (b)
      g.sample.map(Some(_))
    else
      State.unit(None)
  })

  def character: Gen[Char] = choose(32, 128).map(_.toChar)

  def strings: Gen[String] = choose(1, 200).flatMap(n => listOfN(n, character).map(_.foldLeft("")(_ + _)))

  def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) => randomStream(g)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(List(a.toString), i)
      } catch {
        case e: Exception => Falsified(List(buildMsg(a, e)), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    def loop(g: Gen[A], rng: RNG): Stream[A] = {
      val (a, newRng) =  g.sample.run(rng)
      a #:: loop(g, newRng)
    }
    loop(g, rng)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"""Test case: ${s}
       |  generated an exception: ${e.getMessage}
       |  stack trace:
       |  ${e.getStackTrace.mkString("\n")}""".stripMargin

  import IntPimp._

  def choose(from: Int, until: Int): Gen[Int] = Gen(State(RNG.int).map(i => from + (i +% (until - from))))

  def choose2(from: Int, until: Int): Gen[(Int,Int)] = listOfN(2, choose(from,until)).map { case a :: b :: Nil => (a, b) }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = weighted((g1, 0.5),(g2,0.5))

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val Sensitivity = 1000
    val total = (g1._2 + g2._2) * Sensitivity
    choose(0, Sensitivity).flatMap { v =>
      if (total / v < g1._2)
        g1._1
      else
        g2._1
    }
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { i => Gen.listOfN(i, g) }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen { i => Gen.listOfN(math.max(i, 1), g) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take(math.min(n, max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

}

object PropBasedTesting {

  val smallInt = Gen.choose(-10,10)

  val maxProp = Gen.forAll(Gen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val sortedProp = Gen.forAll(Gen.listOf(smallInt)) { ns =>
    val s = ns.sorted
    val (allGreater, last) = s.foldLeft((true, Int.MinValue)) {
      case ((goodSoFar,prev),thisOne) => (goodSoFar && (prev <= thisOne), thisOne)
    }
    allGreater
  }

}
