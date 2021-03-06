/**
  * Created by am_dev on 6/28/16.
  */
import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DCanvas._
import doodle.backend.StandardInterpreter._
import doodle.turtle._
import doodle.turtle.Instruction._

object Week5 {
  def simpleSquare(): Image = {
    val instructions =
      List(forward(10), turn(90.degrees),
        forward(10), turn(90.degrees),
        forward(10), turn(90.degrees),
        forward(10))

    Turtle.draw(instructions)
  }

  def polygon(sides: Int, sideLength: Double): Image = {
    val rotation = Angle.one / sides
    def iter(n: Int): List[Instruction] = {
      n match {
        case 0 => Nil
        case n => turn(rotation) :: forward(sideLength) :: iter(n - 1)
      }
    }

    Turtle.draw(iter(sides))
  }

  def spiralSquare(iterations: Int, rotation: Double): Image = {
    def iter(n: Int): List[Instruction] = {
      n match {
        case 0 => Nil
        case n => turn(rotation.degrees) :: forward((iterations - n) * 2) :: iter(n - 1)
      }
    }
    Turtle.draw(iter(iterations))
  }

  def double[A](in: List[A]): List[A] = {
    in.flatMap { x => List(x, x) }
  }

/*
  def rewrite(instructions: List[Instruction], rule: Instruction => List[Instruction]): List[Instruction] = {
    instructions.flatMap { x => List(rule) }

  }
*/

}

object Branch {
  val stepSize = 10

  def rule(i: Instruction): List[Instruction] = {
    i match {
      case For(_) => List(forward(stepSize), forward(stepSize))
      case NoOp =>
        List(branch(turn(45.degrees), forward(stepSize), noop),
          branch(turn(-45.degrees), forward(stepSize), noop))
      case other => List(other)
    }
  }
}
