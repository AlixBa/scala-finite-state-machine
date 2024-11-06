package com.github.alixba.fsm.telemetry

import cats.FlatMap
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps
import com.github.alixba.fsm.framework.*
import com.github.alixba.fsm.framework.StateAlgebra.*
import org.typelevel.log4cats.SelfAwareStructuredLogger

trait StateRecursiveTextLogger[A] {
  def logR(a: A): A
}

object StateRecursiveTextLogger {

  def L[F[_]](using
      logger: SelfAwareStructuredLogger[F]
  ): SelfAwareStructuredLogger[F] =
    logger

  given state0Logger[F[_]: FlatMap: SelfAwareStructuredLogger, S, O]
      : StateRecursiveTextLogger[State0Algebra[F, S, O]] =
    (algebra: State0Algebra[F, S, O]) =>
      new State0Algebra[F, S, O] {
        override def entryAction: Action[F, S] = (s: S) =>
          for {
            _ <- L[F].debug(s"--> Applying entry action on [$s]")
            e <- algebra.entryAction(s)
            _ <- L[F].debug(s"<-- Entry action applied on [$s]")
          } yield e

        override def output: Output[S, O] = algebra.output
      }

  given state1Logger[
      F[_]: FlatMap: SelfAwareStructuredLogger,
      S0,
      I,
      S1,
      O1,
      A1 <: StateAlgebra[F, S1, O1]
  ](using
      log1: StateRecursiveTextLogger[A1]
  ): StateRecursiveTextLogger[State1Algebra[F, S0, I, S1, O1, A1]] =
    (algebra: State1Algebra[F, S0, I, S1, O1, A1]) =>
      new State1Algebra[F, S0, I, S1, O1, A1] {
        override def entryAction: Action[F, S0] = (s0: S0) =>
          for {
            _ <- L[F].debug(s"--> Applying entry action on [$s0]")
            e <- algebra.entryAction(s0)
            _ <- L[F].debug(s"<-- Entry action applied on [$s0]")
          } yield e

        override def input: Input[F, S0, I] = (s0: S0) =>
          for {
            _ <- L[F].debug(s"--> Calling input on [$s0]")
            i <- algebra.input(s0)
            _ <- L[F].debug(s"<-- Got input [$i] on [$s0]")
          } yield i

        override def transition: Transition[S0, I, S1] = algebra.transition

        override def exitAction: Action[F, (S0, S1)] = (s0: S0, s1: S1) =>
          for {
            _ <- L[F].debug(s"--> Applying exit action on [$s0]->[$s1]")
            u <- algebra.exitAction(s0, s1)
            _ <- L[F].debug(s"<-- Exit action applied on [$s0]->[$s1]")
          } yield u

        override def next: A1 = log1.logR(algebra.next)
      }

  given state2Logger[
      F[_]: FlatMap: SelfAwareStructuredLogger,
      S0,
      I,
      S1,
      O1,
      S2,
      O2,
      A1 <: StateAlgebra[F, S1, O1],
      A2 <: StateAlgebra[F, S2, O2]
  ](using
      log1: StateRecursiveTextLogger[A1],
      log2: StateRecursiveTextLogger[A2]
  ): StateRecursiveTextLogger[State2Algebra[F, S0, I, S1, O1, S2, O2, A1, A2]] =
    (algebra: State2Algebra[F, S0, I, S1, O1, S2, O2, A1, A2]) =>
      new State2Algebra[F, S0, I, S1, O1, S2, O2, A1, A2] {
        override def entryAction: Action[F, S0] = (s0: S0) =>
          for {
            _ <- L[F].debug(s"--> Applying entry action on [$s0]")
            e <- algebra.entryAction(s0)
            _ <- L[F].debug(s"<-- Entry action applied on [$s0]")
          } yield e

        override def input: Input[F, S0, I] = (s0: S0) =>
          for {
            _ <- L[F].debug(s"--> Calling input on [$s0]")
            i <- algebra.input(s0)
            _ <- L[F].debug(s"<-- Got input [$i] on [$s0]")
          } yield i

        override def transition: Transition[S0, I, S1 | S2] = algebra.transition

        override def exitAction: Action[F, (S0, S1 | S2)] =
          (s0: S0, s1s2: S1 | S2) =>
            for {
              _ <- L[F].debug(s"--> Applying exit action on [$s0]->[$s1s2]")
              u <- algebra.exitAction(s0, s1s2)
              _ <- L[F].debug(s"<-- Exit action applied on [$s0]->[$s1s2]")
            } yield u

        override def next1: A1 = log1.logR(algebra.next1)
        override def next2: A2 = log2.logR(algebra.next2)
      }

  given stateProgramLogger[
      F[_]: FlatMap: SelfAwareStructuredLogger,
      S,
      O,
      A <: StateAlgebra[F, S, O]
  ](using
      logA: StateRecursiveTextLogger[A]
  ): StateRecursiveTextLogger[StateProgram[F, S, O, A]] =
    (program: StateProgram[F, S, O, A]) =>
      new StateProgram[F, S, O, A] {
        override def run(algebra: A)(s: S): F[O] =
          for {
            _ <- L[F].info(s"--> Running program on [$s]")
            o <- program.run(logA.logR(algebra))(s)
            _ <- L[F].info(s"<-- Ran program on [$s] with output [$o]")
          } yield o
      }

  given stateWithAlgebraLogger[
      F[_]: FlatMap: SelfAwareStructuredLogger,
      S,
      O,
      A <: StateAlgebra[F, S, O]
  ](using
      log: StateRecursiveTextLogger[StateProgram[F, S, O, A]]
  ): StateRecursiveTextLogger[StateProgram.WithAlgebra[F, S, O, A]] =
    (program: StateProgram.WithAlgebra[F, S, O, A]) =>
      new StateProgram.WithAlgebra[F, S, O, A] {
        override def algebra: A = program.algebra
        override def run(algebra: A)(s: S): F[O] =
          log.logR(program).run(algebra)(s)
      }

}
