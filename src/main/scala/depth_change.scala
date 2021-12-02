import scala.io.Source
import scala.annotation.tailrec
import scala.annotation.tailrec
import scala.math._

@main def calculateDepth: Unit =
  val filename = "depth_input.txt"
  val inputArray = Source.fromResource(filename).getLines.toList.map(_.toInt)
  inputArray match
    case firstElement :: rest =>
      println(foo(accum = 0, currentElement = firstElement, remaining = rest))
    case _ => println("Empty list")

  inputArray match
    case e1 :: e2 :: e3 :: rest =>
      println(bar(0, List(e1, e2, e3).sum, e2 :: e3 :: rest))
    case _ => println("Empty List")

@tailrec def foo(accum: Int, currentElement: Int, remaining: List[Int]): Int =
  remaining match
    case Nil                             => accum
    case e :: rest if e > currentElement => foo(accum + 1, e, rest)
    case e :: rest                       => foo(accum, e, rest)

@tailrec def bar(accum: Int, currentSum: Int, remaining: List[Int]): Int =
  remaining match
    case e1 :: e2 :: e3 :: rest if List(e1, e2, e3).sum > currentSum =>
      bar(accum + 1, List(e1, e2, e3).sum, e2 :: e3 :: rest)
    case e1 :: e2 :: e3 :: rest =>
      bar(accum, List(e1, e2, e3).sum, e2 :: e3 :: rest)
    case _ => accum
