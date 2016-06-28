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

  def spiralSquare(iterations: Int, rotation: Int): Image = {
    def iter(n: Int): List[Instruction] = {
      n match {
        case 0 => Nil
        case iterations => turn(90.degrees) :: forward(n - 1) :: turn ::iter(n - 1)
      }
    }
  }

  def double[A](in: List[A]): List[A] = {
    in.flatMap { x => List(x, x) }
  }

  def rewrite(instructions: List[Instruction], rule: Instruction => List[Instruction]): List[Instruction] = {
    instructions.flatMap { x => List(rule) }

  }
}