// BasicGravityTest.scala
package com.jaketimothy.estimator.planetmodel

import org.scalatest.FunSuite

import breeze.linalg._
import breeze.numerics._

class BasicGravityTest extends FunSuite {

  val limit = 0.005 // percent difference

  val s: Stream[Double] = 1.0 #:: s.map(x => x / 2.0)
  val eps = s.takeWhile(e => e + 1.0 != 1.0).last

  val space = 100000.0 // altitude of Karman line
  val moon = 384399000.0 // distance to Moon (average)

  val pointModel = new PointWGS84Earth()
  val ellipsoidalModel = new EllipsoidalWGS84Earth()

  val a = ellipsoidalModel.referenceEllipsoid.semimajorAxis


  test("gravity x-direction") {

    // edge of space in x-direction
    val positionX = DenseVector(space + a, 0.0, 0.0)
    val pointX = pointModel.gravityModel.gravitationalAcceleration(positionX)
    val ellipsoidalX = ellipsoidalModel.gravityModel.gravitationalAcceleration(positionX)

    val pointXNeg = pointModel.gravityModel.gravitationalAcceleration(-positionX)
    val ellipsoidalXNeg = ellipsoidalModel.gravityModel.gravitationalAcceleration(-positionX)

    assert(pointX(1).toString() != "NaN" && pointX(2).toString() != "NaN")
    assert(ellipsoidalX(1).toString() != "NaN" && ellipsoidalX(2).toString() != "NaN")
    assert(abs((ellipsoidalX(0) - pointX(0)) / pointX(0)) < limit)
    assert(pointX(0) == -pointXNeg(0))
    assert(ellipsoidalX(0) == -ellipsoidalXNeg(0))
  }

  test("gravity y-direction") {

    // edge of space in y-direction
    val positionY = DenseVector(0.0, space + a, 0.0)
    val pointY = pointModel.gravityModel.gravitationalAcceleration(positionY)
    val ellipsoidalY = ellipsoidalModel.gravityModel.gravitationalAcceleration(positionY)

    val pointYNeg = pointModel.gravityModel.gravitationalAcceleration(-positionY)
    val ellipsoidalYNeg = ellipsoidalModel.gravityModel.gravitationalAcceleration(-positionY)

    assert(pointY(0).toString() != "NaN" && pointY(2).toString() != "NaN")
    assert(ellipsoidalY(0).toString() != "NaN" && ellipsoidalY(2).toString() != "NaN")
    assert(abs((ellipsoidalY(1) - pointY(1)) / pointY(1)) < limit)
    assert(pointY(1) == -pointYNeg(1))
    assert(ellipsoidalY(1) == -ellipsoidalYNeg(1))
  }

  test("gravity z-direction") {

    // edge of space in z-direction
    val positionZ = DenseVector(0.0, 0.0, space + a)
    val pointZ = pointModel.gravityModel.gravitationalAcceleration(positionZ)
    val ellipsoidalZ = ellipsoidalModel.gravityModel.gravitationalAcceleration(positionZ)

    val pointZNeg = pointModel.gravityModel.gravitationalAcceleration(-positionZ)
    val ellipsoidalZNeg = ellipsoidalModel.gravityModel.gravitationalAcceleration(-positionZ)

    assert(pointZ(0).toString() != "NaN" && pointZ(1).toString() != "NaN")
    assert(ellipsoidalZ(0).toString() != "NaN" && ellipsoidalZ(1).toString() != "NaN")
    assert(abs((ellipsoidalZ(2) - pointZ(2)) / pointZ(2)) < limit)
    assert(pointZ(2) == -pointZNeg(2))
    assert(ellipsoidalZ(2) == -ellipsoidalZNeg(2))
  }

  test("gravity xyz") {

    val position = DenseVector(space + a, space + a, space + a)
    val point = pointModel.gravityModel.gravitationalAcceleration(position)
    val ellipsoidal = ellipsoidalModel.gravityModel.gravitationalAcceleration(position)

    assert(abs((ellipsoidal(0) - point(0)) / point(0)) < 4 * limit)
    assert(abs((ellipsoidal(1) - point(1)) / point(1)) < 4 * limit)
    assert(abs((ellipsoidal(2) - point(2)) / point(2)) < 4 * limit)
  }
}