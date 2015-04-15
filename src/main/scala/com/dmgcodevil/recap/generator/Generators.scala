package com.dmgcodevil.recap.generator

object Generators {

  trait Generator[+T] {
    self =>
    def generate: T

    def foreach[U](f: T => U) {
      f(generate)
    }

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }

  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)

  val pairDesugared: Generator[(Int, Int)] = integers flatMap {
    x => integers map { y => (x, y)}
  }

  def main(args: Array[String]) {
    print(pairDesugared.generate)
  }
}
