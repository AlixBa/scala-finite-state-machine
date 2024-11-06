package com.github.alixba.fsm.framework

import munit.FunSuite
import com.github.alixba.fsm.TestFSM.*
import com.github.alixba.fsm.syntax.framework.program

import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Success

class StateProgramTest extends FunSuite {

  test("state0") {
    val hasCalledEntryAction = AtomicBoolean(false)
    val out = Rejected()

    val result = onRejected(
      _ => Success(hasCalledEntryAction.set(true)),
      _ => out
    ).program.run(Rejected())

    assertEquals(hasCalledEntryAction.get(), true)
    assertEquals(result, Success(out))
  }

  test("state1") {
    val hasCalledEntryAction = AtomicBoolean(false)
    val hasCalledExitAction = AtomicBoolean(false)

    val result = onInitialState(
      _ => Success(hasCalledEntryAction.set(true)),
      _ => Success(InitOK()),
      _ => Success(hasCalledExitAction.set(true))
    ).program.run(InitialState())

    assertEquals(hasCalledEntryAction.get(), true)
    assertEquals(hasCalledExitAction.get(), true)
    assertEquals(result, Success(RetrievingAccount()))
  }

  test("state2 next1") {
    val hasCalledEntryAction = AtomicBoolean(false)
    val hasCalledExitAction = AtomicBoolean(false)

    val result = onHoldingFunds(
      _ => Success(hasCalledEntryAction.set(true)),
      _ => Success(FundsKO()),
      _ => Success(hasCalledExitAction.set(true))
    ).program.run(HoldingFunds())

    assertEquals(hasCalledEntryAction.get(), true)
    assertEquals(hasCalledExitAction.get(), true)
    assertEquals(result, Success(Rejected()))
  }

  test("state2 next2") {
    val hasCalledEntryAction = AtomicBoolean(false)
    val hasCalledExitAction = AtomicBoolean(false)

    val result = onHoldingFunds(
      _ => Success(hasCalledEntryAction.set(true)),
      _ => Success(FundsOK()),
      _ => Success(hasCalledExitAction.set(true))
    ).program.run(HoldingFunds())

    assertEquals(hasCalledEntryAction.get(), true)
    assertEquals(hasCalledExitAction.get(), true)
    assertEquals(result, Success(Accepted()))
  }

}
