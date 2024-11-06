package com.github.alixba.fsm.framework

sealed trait StateAlgebra[F[_], S, O]

object StateAlgebra {

  trait State0Algebra[F[_], S, O] extends StateAlgebra[F, S, O] {
    def entryAction: Action[F, S]
    def output: Output[S, O]
  }

  trait State1Algebra[
      F[_],
      S0,
      I,
      S1,
      O1,
      A <: StateAlgebra[F, S1, O1]
  ] extends StateAlgebra[F, S0, O1] {
    def entryAction: Action[F, S0]
    def input: Input[F, S0, I]
    def transition: Transition[S0, I, S1]
    def exitAction: Action[F, (S0, S1)]
    def next: A
  }

  trait State2Algebra[
      F[_],
      S0,
      I,
      S1,
      O1,
      S2,
      O2,
      A1 <: StateAlgebra[F, S1, O1],
      A2 <: StateAlgebra[F, S2, O2]
  ] extends StateAlgebra[F, S0, O1 | O2] {
    def entryAction: Action[F, S0]
    def input: Input[F, S0, I]
    def transition: Transition[S0, I, S1 | S2]
    def exitAction: Action[F, (S0, S1 | S2)]
    def next1: A1
    def next2: A2
  }

}
