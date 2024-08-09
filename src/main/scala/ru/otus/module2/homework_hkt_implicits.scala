package ru.otus.module2

import scala.language.implicitConversions

object homework_hkt_implicits {

  trait Bindable[F[_], A]{
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  object Bindable {
    implicit def optToBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
      override def map[B](f: A => B): Option[B] = opt.map(f)
      override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
    }

    implicit def seqToBindable[A](seq: Seq[A]): Bindable[Seq, A] = new Bindable[Seq, A] {
      override def map[B](f: A => B): Seq[B] = seq.map(f)
      override def flatMap[B](f: A => Seq[B]): Seq[B] = seq.flatMap(f)
    }
  }

  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(
    implicit faToBindable: F[A] => Bindable[F, A], // conditional implicit
             fbToBindable: F[B] => Bindable[F, B]
  ): F[(A, B)] =
    fa.flatMap { a => fb.map((a, _)) }

  // println(tuplef(Some(5), Some("str"))) exception: No implicit view available from Some[Int] => Bindable[Some,Int]
  println(tuplef(Option(5), Some("str")))
  println(tuplef(Some(5), None))
  println(tuplef(Seq(1, 2, 3), Seq("!")))
}

object App {
  def main(args: Array[String]): Unit = {
    homework_hkt_implicits
  }
}