/**
  * Created by am_dev on 6/7/16.
  */

import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DCanvas._
import doodle.backend.StandardInterpreter._

object Week4 {
  def parametricCircle (angle: Angle): Point = {
    Point.cartesian(angle.cos, angle.sin)
  }

  def sample (start: Angle, increment: Angle, design: Angle => Point, shape: Int => Image): Image = {
    if(start > Angle.one) {
      Image.empty
    } else {
      val pt = design(start)
      shape(1) at (Point.polar(pt.r * 100, pt.angle).toVec) on sample(start + increment, increment, design, shape)
    }
  }
  //Week4.sample(0.degrees, 1.degrees, Week4.rose _, Week4.triangle _).draw

  def rose(angle: Angle) = {
    Point.cartesian((angle * 7).cos * angle.cos, (angle * 7).cos * angle.sin)
  }

  def scale(factor: Double): Point => Point = {
    (pt: Point) => Point.polar(pt.r * factor, pt.angle)
  }

  def cascadingShapes(nDepth: Int, shape: Int => Image): Image = {
    if(nDepth == 1) {
      shape(nDepth)
    } else {
      shape(nDepth) on cascadingShapes(nDepth - 1, shape)
    }
  }

  def outlinedCircle(n: Int) = {
    Circle(n * 10)
  }

  def circleOrSquare(n: Int) = {
    if(n % 2 == 0) Rectangle(n*20, n*20) else Circle(n*10)
  }

  def size(n: Int): Double = {
    //50 + 12 * n
    n * 10
  }

  def circle(n: Int): Image = {
    Circle(size(n))
  }

  def square(n: Int): Image = {
    Rectangle(2 * size(n), 2 * size(n))
  }

  def triangle(n: Int): Image = {
    Triangle(2 * size(n), 2 * size(n))
  }

  def fading(n: Int): Color = {
    Color.blue fadeOut (1 - n / 20.0).normalized
  }

  def spinning(n: Int): Color = {
    Color.blue desaturate 0.5.normalized spin (n * 30).degrees
  }

  def colored(shape: Int => Image, color: Int => Color): Int => Image = {
    (n: Int) => shape(n) lineWidth 10 lineColor color(n)
  }

  val answer =
    ( cascadingShapes(10, colored(circle, spinning)) beside
      cascadingShapes(10, colored(triangle, fading)) beside
      cascadingShapes(10, colored(square, spinning)))

}
