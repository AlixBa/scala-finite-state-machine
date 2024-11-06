package com.github.alixba.fsm.framework

import com.github.alixba.fsm.framework.StateAlgebra.*

import scala.util.NotGiven

trait StateAlgebraMerger[
    F[_],
    S1,
    O1,
    S2,
    O2,
    A1 <: StateAlgebra[F, S1, O1],
    A2 <: StateAlgebra[F, S2, O2]
] {
  type Out
  type A3 <: StateAlgebra[F, S1, Out]

  def apply(algebra1: A1, algebra2: A2): A3
}

object StateAlgebraMerger {

  type Aux[
      F[_],
      S1,
      O1,
      S2,
      O2,
      A1 <: StateAlgebra[F, S1, O1],
      A2 <: StateAlgebra[F, S2, O2],
      Out0,
      A30 <: StateAlgebra[F, S1, Out0]
  ] = StateAlgebraMerger[F, S1, O1, S2, O2, A1, A2] {
    type A3 = A30
    type Out = Out0
  }

  def apply[
      F[_],
      S1,
      O1,
      S2,
      O2,
      A1 <: StateAlgebra[F, S1, O1],
      A2 <: StateAlgebra[F, S2, O2]
  ](using
      merger: StateAlgebraMerger[F, S1, O1, S2, O2, A1, A2]
  ): Aux[F, S1, O1, S2, O2, A1, A2, merger.Out, merger.A3] =
    merger

  given state0Merger[
      F[_],
      S,
      O1,
      O2,
      A1 <: State0Algebra[F, S, O1],
      A2 <: StateAlgebra[F, S, O2]
  ]: Aux[F, S, O1, S, O2, A1, A2, O2, A2] =
    new StateAlgebraMerger[F, S, O1, S, O2, A1, A2] {
      override type Out = O2
      override type A3 = A2

      override def apply(algebra1: A1, algebra2: A2): A3 = algebra2
    }

  given state1Merger[
      F[_],
      S0,
      I,
      S1,
      O1,
      S2,
      O2,
      A11 <: StateAlgebra[F, S1, O1],
      A1 <: State1Algebra[F, S0, I, S1, O1, A11],
      A2 <: StateAlgebra[F, S2, O2]
  ](using
      merger: StateAlgebraMerger[F, S1, O1, S2, O2, A11, A2]
  ): Aux[
    F,
    S0,
    O1,
    S2,
    O2,
    A1,
    A2,
    merger.Out,
    State1Algebra[F, S0, I, S1, merger.Out, merger.A3]
  ] = new StateAlgebraMerger[F, S0, O1, S2, O2, A1, A2] {
    override type Out = merger.Out
    override type A3 = State1Algebra[F, S0, I, S1, merger.Out, merger.A3]

    override def apply(_algebra1: A1, algebra2: A2): A3 =
      new State1Algebra[F, S0, I, S1, merger.Out, merger.A3] {
        override def entryAction: Action[F, S0] = _algebra1.entryAction
        override def input: Input[F, S0, I] = _algebra1.input
        override def transition: Transition[S0, I, S1] = _algebra1.transition
        override def exitAction: Action[F, (S0, S1)] = _algebra1.exitAction
        override def next: merger.A3 = merger(_algebra1.next, algebra2)
      }
  }

  given state2AMerger[
      F[_],
      S0,
      I,
      S11,
      O11,
      S12,
      O12,
      S2,
      O2,
      A11 <: StateAlgebra[F, S11, O11],
      A12 <: StateAlgebra[F, S12, O12],
      A1 <: State2Algebra[F, S0, I, S11, O11, S12, O12, A11, A12],
      A2 <: StateAlgebra[F, S2, O2]
  ](using
      merger1: StateAlgebraMerger[F, S11, O11, S2, O2, A11, A2],
      ev: NotGiven[StateAlgebraMerger[F, S12, O12, S2, O2, A12, A2]]
  ): Aux[
    F,
    S0,
    O11 | O12,
    S2,
    O2,
    A1,
    A2,
    merger1.Out | O12,
    State2Algebra[
      F,
      S0,
      I,
      S11,
      merger1.Out,
      S12,
      O12,
      merger1.A3,
      A12
    ]
  ] = new StateAlgebraMerger[F, S0, O11 | O12, S2, O2, A1, A2] {
    override type Out = merger1.Out | O12
    override type A3 = State2Algebra[
      F,
      S0,
      I,
      S11,
      merger1.Out,
      S12,
      O12,
      merger1.A3,
      A12
    ]

    override def apply(_algebra1: A1, _algebra2: A2): A3 =
      new State2Algebra[
        F,
        S0,
        I,
        S11,
        merger1.Out,
        S12,
        O12,
        merger1.A3,
        A12
      ] {
        override def entryAction: Action[F, S0] = _algebra1.entryAction
        override def input: Input[F, S0, I] = _algebra1.input
        override def transition: Transition[S0, I, S11 | S12] =
          _algebra1.transition
        override def exitAction: Action[F, (S0, S11 | S12)] =
          _algebra1.exitAction
        override def next1: merger1.A3 = merger1(_algebra1.next1, _algebra2)
        override def next2: A12 = _algebra1.next2
      }
  }

  given state2BMerger[
      F[_],
      S0,
      I,
      S11,
      O11,
      S12,
      O12,
      S2,
      O2,
      A11 <: StateAlgebra[F, S11, O11],
      A12 <: StateAlgebra[F, S12, O12],
      A1 <: State2Algebra[F, S0, I, S11, O11, S12, O12, A11, A12],
      A2 <: StateAlgebra[F, S2, O2]
  ](using
      ev: NotGiven[StateAlgebraMerger[F, S11, O11, S2, O2, A11, A2]],
      merger2: StateAlgebraMerger[F, S12, O12, S2, O2, A12, A2]
  ): Aux[
    F,
    S0,
    O11 | O12,
    S2,
    O2,
    A1,
    A2,
    O11 | merger2.Out,
    State2Algebra[
      F,
      S0,
      I,
      S11,
      O11,
      S12,
      merger2.Out,
      A11,
      merger2.A3
    ]
  ] = new StateAlgebraMerger[F, S0, O11 | O12, S2, O2, A1, A2] {
    override type Out = O11 | merger2.Out
    override type A3 = State2Algebra[
      F,
      S0,
      I,
      S11,
      O11,
      S12,
      merger2.Out,
      A11,
      merger2.A3
    ]

    override def apply(_algebra1: A1, _algebra2: A2): A3 =
      new State2Algebra[
        F,
        S0,
        I,
        S11,
        O11,
        S12,
        merger2.Out,
        A11,
        merger2.A3
      ] {
        override def entryAction: Action[F, S0] = _algebra1.entryAction
        override def input: Input[F, S0, I] = _algebra1.input
        override def transition: Transition[S0, I, S11 | S12] =
          _algebra1.transition
        override def exitAction: Action[F, (S0, S11 | S12)] =
          _algebra1.exitAction
        override def next1: A11 = _algebra1.next1
        override def next2: merger2.A3 = merger2(_algebra1.next2, _algebra2)
      }
  }

  given state2ABMerger[
      F[_],
      S0,
      I,
      S11,
      O11,
      S12,
      O12,
      S2,
      O2,
      A11 <: StateAlgebra[F, S11, O11],
      A12 <: StateAlgebra[F, S12, O12],
      A1 <: State2Algebra[F, S0, I, S11, O11, S12, O12, A11, A12],
      A2 <: StateAlgebra[F, S2, O2]
  ](using
      merger1: StateAlgebraMerger[F, S11, O11, S2, O2, A11, A2],
      merger2: StateAlgebraMerger[F, S12, O12, S2, O2, A12, A2]
  ): Aux[
    F,
    S0,
    O11 | O12,
    S2,
    O2,
    A1,
    A2,
    merger1.Out | merger2.Out,
    State2Algebra[
      F,
      S0,
      I,
      S11,
      merger1.Out,
      S12,
      merger2.Out,
      merger1.A3,
      merger2.A3
    ]
  ] = new StateAlgebraMerger[F, S0, O11 | O12, S2, O2, A1, A2] {
    override type Out = merger1.Out | merger2.Out
    override type A3 = State2Algebra[
      F,
      S0,
      I,
      S11,
      merger1.Out,
      S12,
      merger2.Out,
      merger1.A3,
      merger2.A3
    ]

    override def apply(_algebra1: A1, _algebra2: A2): A3 =
      new State2Algebra[
        F,
        S0,
        I,
        S11,
        merger1.Out,
        S12,
        merger2.Out,
        merger1.A3,
        merger2.A3
      ] {
        override def entryAction: Action[F, S0] = _algebra1.entryAction
        override def input: Input[F, S0, I] = _algebra1.input
        override def transition: Transition[S0, I, S11 | S12] =
          _algebra1.transition
        override def exitAction: Action[F, (S0, S11 | S12)] =
          _algebra1.exitAction
        override def next1: merger1.A3 = merger1(_algebra1.next1, _algebra2)
        override def next2: merger2.A3 = merger2(_algebra1.next2, _algebra2)
      }
  }

}
