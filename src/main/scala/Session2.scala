/**
  * Created by am_dev on 5/24/16.
  */

import doodle.core
import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DCanvas._
import doodle.backend.StandardInterpreter._
import doodle.examples._


object Week2Examples {
  def sequentialBoxes(nBoxes: Int): Image = {
    nBoxes match {
      case 0 => Image.empty
      case n => (rectangle(10, 10) fillColor Color.red) beside sequentialBoxes(n - 1)
    }
  }

  def concentricCircles(nCircles: Int, nRadiusFactor: Int, nColorSpin: Int): Image = {
    nCircles match {
      case 0 => Image.empty
      case nCircles => (circle(nCircles * nRadiusFactor) lineColor Color.blue.spin((nCircles * nColorSpin).degrees)) on concentricCircles(nCircles - 1, nRadiusFactor, nColorSpin)
    }
  }

  def sierpinskiTriangle(nDepth: Int): Image = {
    nDepth match {
      case 0 => triangle(10, 10).fillColor(Color.black)
      case nDepth => (triangle(10, 10) beside triangle(10, 10)).fillColor(Color.black) below sierpinskiTriangle(nDepth - 1)
    }
  }
}

