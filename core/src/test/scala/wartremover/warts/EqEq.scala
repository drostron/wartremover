package org.brianmckenna.wartremover
package test

import org.scalatest.FunSuite

import org.brianmckenna.wartremover.warts.EqEq

class EqEqTest extends FunSuite {
  test("== is disabled") {
    val result = WartTestTraverser(EqEq) {
      val i = 1 == "a"
    }
    assertResult(List("== is disabled"), "result.errors")(result.errors)
    assertResult(List.empty, "result.warnings")(result.warnings)
  }
  test("!= is disabled") {
    val result = WartTestTraverser(EqEq) {
      val i = 1 != "a"
    }
    assertResult(List("!= is disabled"), "result.errors")(result.errors)
    assertResult(List.empty, "result.warnings")(result.warnings)
  }
  test("case class succeeds") {
    val result = WartTestTraverser(EqEq) {
      case class Z()
    }
    assertResult(List.empty, "result.errors")(result.errors)
    assertResult(List.empty, "result.warnings")(result.warnings)
  }
  test("pattern match succeeds") {
    val result = WartTestTraverser(EqEq) {
      val i = List(1)
      val j = i match {
        case Nil if 1 != "a" => "yes"
        case _ => "no"
      }
    }
    assertResult(List("!= is disabled"), "result.errors")(result.errors)
    assertResult(List.empty, "result.warnings")(result.warnings)
  }
  test("EqEq wart obeys SuppressWarnings") {
    val result = WartTestTraverser(EqEq) {
      @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.EqEq"))
      val i = {
        1 == "a"
      }
    }
    assertResult(List.empty, "result.errors")(result.errors)
    assertResult(List.empty, "result.warnings")(result.warnings)
  }
}
