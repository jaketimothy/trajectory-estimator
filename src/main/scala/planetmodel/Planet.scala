// Planet.scala
package com.jaketimothy.estimator.planetmodel

trait Planet {

  def referenceEllipsoid: ReferenceEllipsoid
  def angularVelocity: Double // radians/second
  def gravitationalParameter: Double // meters^3/seconds^2, exoatmospheric

  def gravityModel: GravityModel
}
