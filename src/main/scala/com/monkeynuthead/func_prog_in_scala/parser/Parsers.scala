package com.monkeynuthead.func_prog_in_scala.parser

import com.monkeynuthead.func_prog_in_scala.props.{Prop, Gen}

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.language.higherKinds
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  //Primitives

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = ???

  implicit def regex(r: Regex): Parser[String] = ???

  def slice[A](p: Parser[A]): Parser[String] = ???

  implicit def string(s: String): Parser[String] = ???

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  //Non-Primitive

  implicit def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0)
      succeed(Nil)
    else
      map2(p, listOfN(n - 1, p))( (a, as) => a :: as )

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))( _ :: _ ) or succeed(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap ( a => succeed(f(a)) )

  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p1 flatMap ( a => p2 map ( b => f(a,b) ) )

  implicit def parserWithOps[A](p: Parser[A]): ParserOps[A] = new ParserOps[A](p)

  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    p1  flatMap ( a => p2  map (b => (a,b)) )

  case class ParserOps[A](p: Parser[A]) {

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)

    def reps(n: Int): Parser[List[A]] = self.listOfN[A](n, p)

    def slice: Parser[String] = self.slice(p)

  }

  object Laws {

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def succeedLaw[A, B](in1: Gen[A], in2: Gen[String]): Prop = {
      Gen.forAll(in1.flatMap(a => in2.flatMap(b => Gen.unit((a, b))))) {
        case (a, s) => run(succeed(a))(s) == Right(a)
      }
    }

  }

  val numA: Parser[Int] = char('a').slice.map(_.size)

  val manyAs = regex("[0-9]*".r) flatMap ( n => listOfN(n.toInt, char('a')) )

}

object Main extends Parsers {

  def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice    
  }


  def main(args: Array[String] = Array()): Unit = {

    run('C')("C") == Right('C')

    run("abracadabra")("abracadabra") == Right("abracadabra")

    run("abra" | "cadabra")("abra") == Right("abra")
    run("abra" | "cadabra")("cadabra") == Right("cadabra")

    run(("ab" | "cd") reps 3)("ababcad") = Right("ababcad")
    run(("ab" | "cd") reps 3)("cadabab") = Right("cadabab")
    run(("ab" | "cd") reps 3)("ababab") = Right("ababab")

    run(numA)("aaa") == Right(3)
    run(numA)("b") == Right(0)

    //zero or more 'a' followed by 1 or more 'b'
    char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

  }

}