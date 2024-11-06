package com.github.alixba.fsm.framework

import com.github.alixba.fsm.TestFSM.*
import com.github.alixba.fsm.framework.StateAlgebra.State0Algebra
import com.github.alixba.fsm.syntax.framework.merge
import munit.{Compare, FunSuite}

import scala.reflect.ClassTag
import scala.util.Try

class StateAlgebraMergerTest extends FunSuite {

  implicit def state0Compare[LS, LO, RS, RO]
      : Compare[State0Algebra[Try, LS, LO], State0Algebra[Try, RS, RO]] =
    (
        obtained: State0Algebra[Try, LS, LO],
        expected: State0Algebra[Try, RS, RO]
    ) =>
      obtained.entryAction == expected.entryAction &&
        obtained.output == expected.output

  test("non matching state0") {
    val left = onRejected()
    val right = onAccepted()

    compileErrors("left.merge(right)")
  }

  test("matching state0") {
    val left = onRejected()
    val right = onRejected()

    assertEquals(left.merge(right), right)
  }

  test("non matching state1") {
    val left = onInitialState()
    val right = onRejected()

    compileErrors("left.merge(right)")
  }

  test("matching state1") {
    val left = onInitialState()
    val right = onRetrievingAccount()
    val merge = left.merge(right)

    assertEquals(merge.entryAction, left.entryAction)
    assertEquals(merge.input, left.input)
    assertEquals(merge.transition, left.transition)
    assertEquals(merge.exitAction, left.exitAction)
    assertEquals(merge.next, right)
  }

  test("non matching state2") {
    val left = onRetrievingAccount()
    val right = onAccepted()

    compileErrors("left.merge(right)")
  }

  test("matching next1 state2") {
    val left = onHoldingFunds()
    val right = onRejected()
    val merge = left.merge(right)

    assertEquals(merge.entryAction, left.entryAction)
    assertEquals(merge.input, left.input)
    assertEquals(merge.transition, left.transition)
    assertEquals(merge.exitAction, left.exitAction)
    assertEquals(merge.next1, right)
    // TODO this should work without the custom compare at the top
    assertEquals(merge.next2, left.next2)
  }

  test("matching next2 state2") {
    val left = onHoldingFunds()
    val right = onAccepted()
    val merge = left.merge(right)

    assertEquals(merge.entryAction, left.entryAction)
    assertEquals(merge.input, left.input)
    assertEquals(merge.transition, left.transition)
    assertEquals(merge.exitAction, left.exitAction)
    // TODO this should work without the custom compare at the top
    assertEquals(merge.next1, merge.next1)
    assertEquals(merge.next2, right)
  }

  // todo matching next1&next2 state2

}
