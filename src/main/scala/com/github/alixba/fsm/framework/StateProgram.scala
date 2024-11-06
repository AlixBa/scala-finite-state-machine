package com.github.alixba.fsm.framework

import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps
import cats.{FlatMap, Functor}
import com.github.alixba.fsm.framework.StateAlgebra.*

import scala.reflect.ClassTag

trait StateProgram[F[_], S, O, A <: StateAlgebra[F, S, O]] {
  def run(algebra: A)(s: S): F[O]
}

object StateProgram {

  trait WithAlgebra[F[_], S, O, A <: StateAlgebra[F, S, O]]
      extends StateProgram[F, S, O, A] {
    def algebra: A
    def run(s: S): F[O] = run(algebra)(s)
  }

  given state0Program[F[_]: Functor, S, O]
      : StateProgram[F, S, O, State0Algebra[F, S, O]] =
    new StateProgram[F, S, O, State0Algebra[F, S, O]] {
      override def run(algebra: State0Algebra[F, S, O])(s: S): F[O] =
        algebra.entryAction(s).map(_ => algebra.output(s))
    }

  given state1Program[
      F[_]: FlatMap,
      S0,
      I,
      S1,
      O1,
      A <: StateAlgebra[F, S1, O1]
  ](using
      programA: StateProgram[F, S1, O1, A]
  ): StateProgram[F, S0, O1, State1Algebra[F, S0, I, S1, O1, A]] =
    new StateProgram[F, S0, O1, State1Algebra[F, S0, I, S1, O1, A]] {
      override def run(
          algebra: State1Algebra[F, S0, I, S1, O1, A]
      )(s0: S0): F[O1] =
        for {
          _ <- algebra.entryAction(s0)
          i <- algebra.input(s0)
          s1 = algebra.transition(s0, i)
          _ <- algebra.exitAction((s0, s1))
          o1 <- programA.run(algebra.next)(s1)
        } yield o1
    }

  given state2Program[
      F[_]: FlatMap,
      S0,
      I,
      S1: ClassTag,
      O1,
      S2: ClassTag,
      O2,
      A1 <: StateAlgebra[F, S1, O1],
      A2 <: StateAlgebra[F, S2, O2]
  ](using
      programA1: StateProgram[F, S1, O1, A1],
      programA2: StateProgram[F, S2, O2, A2]
  ): StateProgram[
    F,
    S0,
    O1 | O2,
    State2Algebra[F, S0, I, S1, O1, S2, O2, A1, A2]
  ] =
    new StateProgram[
      F,
      S0,
      O1 | O2,
      State2Algebra[F, S0, I, S1, O1, S2, O2, A1, A2]
    ] {
      override def run(
          algebra: State2Algebra[F, S0, I, S1, O1, S2, O2, A1, A2]
      )(s0: S0): F[O1 | O2] =
        for {
          _ <- algebra.entryAction(s0)
          i <- algebra.input(s0)
          s1s2 = algebra.transition(s0, i)
          _ <- algebra.exitAction((s0, s1s2))
          o1o2 <- s1s2 match {
            case s1: S1 => programA1.run(algebra.next1)(s1).map(identity)
            case s2: S2 => programA2.run(algebra.next2)(s2).map(identity)
          }
        } yield o1o2
    }

}
