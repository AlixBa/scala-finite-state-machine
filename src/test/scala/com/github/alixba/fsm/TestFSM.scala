package com.github.alixba.fsm

import com.github.alixba.fsm.framework.*
import com.github.alixba.fsm.framework.StateAlgebra.*
import com.github.alixba.fsm.syntax.framework.*

import scala.util.{Success, Try}

object TestFSM {

  case class InitialState()
  case class InitOK()

  case class RetrievingAccount()
  case class AccountKO()
  case class AccountOK()

  case class HoldingFunds()
  case class FundsKO()
  case class FundsOK()

  case class Accepted()
  case class Rejected()

  def onRejected(
      action: Action[Try, Rejected] = _ => Success(()),
      _output: Output[Rejected, Rejected] = identity
  ) = new State0Algebra[Try, Rejected, Rejected] {
    override def entryAction: Action[Try, Rejected] = action
    override def output: Output[Rejected, Rejected] = _output
  }

  def onAccepted(
      action: Action[Try, Accepted] = _ => Success(()),
      _output: Output[Accepted, Accepted] = identity
  ) = new State0Algebra[Try, Accepted, Accepted] {
    override def entryAction: Action[Try, Accepted] = action
    override def output: Output[Accepted, Accepted] = _output
  }

  def onHoldingFunds(
      _entryAction: Action[Try, HoldingFunds] = _ => Success(()),
      _input: Input[Try, HoldingFunds, FundsKO | FundsOK] = _ =>
        Success(FundsOK()),
      _exitAction: Action[Try, (HoldingFunds, Rejected | Accepted)] = _ =>
        Success(())
  ) = new State2Algebra[
    Try,
    HoldingFunds,
    FundsKO | FundsOK,
    Rejected,
    Rejected,
    Accepted,
    Accepted,
    State0Algebra[Try, Rejected, Rejected],
    State0Algebra[Try, Accepted, Accepted]
  ] {
    override def entryAction: Action[Try, HoldingFunds] =
      _entryAction
    override def input: Input[Try, HoldingFunds, FundsKO | FundsOK] =
      _input
    override def transition: Transition[
      HoldingFunds,
      FundsKO | FundsOK,
      Rejected | Accepted
    ] = {
      case (_, _: FundsKO) => Rejected()
      case (_, _: FundsOK) => Accepted()
    }
    override def exitAction: Action[Try, (HoldingFunds, Rejected | Accepted)] =
      _exitAction
    override def next1: State0Algebra[Try, Rejected, Rejected] =
      new State0Algebra[Try, Rejected, Rejected] {
        override def entryAction: Action[Try, Rejected] = _ => Success(())
        override def output: Output[Rejected, Rejected] = identity
      }
    override def next2: State0Algebra[Try, Accepted, Accepted] =
      new State0Algebra[Try, Accepted, Accepted] {
        override def entryAction: Action[Try, Accepted] = _ => Success(())
        override def output: Output[Accepted, Accepted] = identity
      }
  }

  def onRetrievingAccount(
      _entryAction: Action[Try, RetrievingAccount] = _ => Success(()),
      _input: Input[Try, RetrievingAccount, AccountKO | AccountOK] = _ =>
        Success(AccountOK()),
      _exitAction: Action[Try, (RetrievingAccount, Rejected | HoldingFunds)] =
        _ => Success(())
  ) = new State2Algebra[
    Try,
    RetrievingAccount,
    AccountKO | AccountOK,
    Rejected,
    Rejected,
    HoldingFunds,
    HoldingFunds,
    State0Algebra[Try, Rejected, Rejected],
    State0Algebra[Try, HoldingFunds, HoldingFunds]
  ] {
    override def entryAction: Action[Try, RetrievingAccount] =
      _entryAction
    override def input: Input[Try, RetrievingAccount, AccountKO | AccountOK] =
      _input
    override def transition: Transition[
      RetrievingAccount,
      AccountKO | AccountOK,
      Rejected | HoldingFunds
    ] = {
      case (_, _: AccountKO) => Rejected()
      case (_, _: AccountOK) => HoldingFunds()
    }
    override def exitAction: Action[
      Try,
      (RetrievingAccount, Rejected | HoldingFunds)
    ] =
      _exitAction
    override def next1: State0Algebra[Try, Rejected, Rejected] =
      new State0Algebra[Try, Rejected, Rejected] {
        override def entryAction: Action[Try, Rejected] = _ => Success(())
        override def output: Output[Rejected, Rejected] = identity
      }
    override def next2: State0Algebra[Try, HoldingFunds, HoldingFunds] =
      new State0Algebra[Try, HoldingFunds, HoldingFunds] {
        override def entryAction: Action[Try, HoldingFunds] = _ => Success(())
        override def output: Output[HoldingFunds, HoldingFunds] = identity
      }
  }

  def onInitialState(
      _entryAction: Action[Try, InitialState] = _ => Success(()),
      _input: Input[Try, InitialState, InitOK] = _ => Success(InitOK()),
      _exitAction: Action[Try, (InitialState, RetrievingAccount)] = _ =>
        Success(())
  ) = new State1Algebra[
    Try,
    InitialState,
    InitOK,
    RetrievingAccount,
    RetrievingAccount,
    State0Algebra[Try, RetrievingAccount, RetrievingAccount]
  ] {
    override def entryAction: Action[Try, InitialState] =
      _entryAction
    override def input: Input[Try, InitialState, InitOK] =
      _input
    override def transition: Transition[
      InitialState,
      InitOK,
      RetrievingAccount
    ] = (_, _) => RetrievingAccount()
    override def exitAction: Action[Try, (InitialState, RetrievingAccount)] =
      _exitAction
    override def next
        : State0Algebra[Try, RetrievingAccount, RetrievingAccount] =
      new State0Algebra[Try, RetrievingAccount, RetrievingAccount] {
        override def entryAction: Action[Try, RetrievingAccount] = _ =>
          Success(())
        override def output: Output[RetrievingAccount, RetrievingAccount] =
          identity
      }
  }

  def fsm(
      initialStateEntryAction: Action[Try, InitialState] = _ => Success(()),
      initialStateInput: Input[Try, InitialState, InitOK] = _ =>
        Success(InitOK()),
      initialStateExitAction: Action[Try, (InitialState, RetrievingAccount)] =
        _ => Success(()),
      retrievingAccountEntryAction: Action[Try, RetrievingAccount] = _ =>
        Success(()),
      retrievingAccountInput: Input[
        Try,
        RetrievingAccount,
        AccountKO | AccountOK
      ] = _ => Success(AccountOK()),
      retrievingAccountExitAction: Action[
        Try,
        (RetrievingAccount, Rejected | HoldingFunds)
      ] = _ => Success(()),
      holdingFundsEntryAction: Action[Try, HoldingFunds] = _ => Success(()),
      holdingFundsInput: Input[Try, HoldingFunds, FundsKO | FundsOK] = _ =>
        Success(FundsOK()),
      holdingFundsExitAction: Action[Try, (HoldingFunds, Rejected | Accepted)] =
        _ => Success(()),
      rejectedAction: Action[Try, Rejected] = _ => Success(()),
      acceptedAction: Action[Try, Accepted] = _ => Success(())
  ) =
    onInitialState(
      initialStateEntryAction,
      initialStateInput,
      initialStateExitAction
    ).merge(
      onRetrievingAccount(
        retrievingAccountEntryAction,
        retrievingAccountInput,
        retrievingAccountExitAction
      )
    ).merge(
      onHoldingFunds(
        holdingFundsEntryAction,
        holdingFundsInput,
        holdingFundsExitAction
      )
    ).merge(onRejected(rejectedAction))
      .merge(onAccepted(acceptedAction))

}
