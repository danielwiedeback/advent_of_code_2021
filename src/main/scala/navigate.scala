import scala.io.Source
import scala.annotation.tailrec

@main def navigate: Unit =
  val filename = "navigation_input.txt"
  val inputArray =
    Source
      .fromResource(filename)
      .getLines
      .toList
      .map { line =>
        val key :: magnitude :: _ = line.split(" ").toList
        (Direction.valueOf(key.capitalize), magnitude.toInt)
      }

  val startingPosition = Position(0, 0, 0)
  val finalPosition = calculatePosition(startingPosition, inputArray)

  println(
    s"horizontal: ${finalPosition.horizontal}, depth: ${finalPosition.depth}, answer = ${finalPosition.horizontal * finalPosition.depth}"
  )

@tailrec def calculatePosition(
    currentPosition: Position,
    remainingDirections: List[(Direction, Int)]
): Position =
  remainingDirections match
    case (Direction.Forward, magnitude) :: rest =>
      val newPosition =
        currentPosition.copy(
          horizontal = currentPosition.horizontal + magnitude,
          depth = currentPosition.depth + (currentPosition.aim * magnitude)
        )
      calculatePosition(newPosition, rest)
    case (Direction.Up, magnitude) :: rest =>
      val newPosition =
        currentPosition.copy(aim = currentPosition.aim - magnitude)
      calculatePosition(newPosition, rest)
    case (Direction.Down, magnitude) :: rest =>
      val newPosition =
        currentPosition.copy(aim = currentPosition.aim + magnitude)
      calculatePosition(newPosition, rest)
    case Nil => currentPosition

case class Position(horizontal: Int, depth: Int, aim: Int)

enum Direction:
  case Forward, Up, Down
