package pl.robo

import pl.robo.Concurrent._

import scala.util.Random

object Main extends App {
  ex0 run

  ex1 run

  def ex0(): Concurrent[Unit] = par(loop(genRandom(1337)), loop(genRandom(2600)) andThen atom(putStrLn("")))

  def ex1(): Concurrent[Unit] = atom(putStr("Haskell")) andThen loop(genRandom(7331)) fork () andThen loop(genRandom(42)) andThen atom(putStrLn(""))

  def genRandom(s: Int): List[Int] = s match {
    case 42 => List(71, 71, 17, 14, 16, 91, 18, 71, 58, 75)
    case 1337 => List(1, 96, 36, 11, 42, 47, 9, 1, 62, 73)
    case 2600 => List(83, 98, 35, 84, 44, 61, 54, 35, 83, 9)
    case 7331 => List(17, 73, 92, 36, 22, 72, 19, 35, 6, 74)
    case _ => {
      val rand = new Random(s)
      (0 to 9).map(_ => rand.nextInt(99)).toList
    }
  }

  def loop(list: List[Int]): Concurrent[Unit] = list.map(String.valueOf(_))
    .map(putStr(_))
    .map(atom(_))
    .foldRight(Concurrent.of())(_ andThen _)

  def putStr(line: String): Unit => Unit = _ => print(line)

  def putStrLn(line: String): Unit => Unit = _ => println(line)

  def putChar(c: Char): Unit => Unit = _ => print(c)
}