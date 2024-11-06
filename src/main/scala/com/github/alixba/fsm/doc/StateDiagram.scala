package com.github.alixba.fsm.doc

import com.github.alixba.fsm.framework.StateAlgebra.*
import com.github.alixba.fsm.framework.{StateAlgebra, StateProgram}

import scala.reflect.ClassTag

trait StateDiagram[A] {
  def diagram: Seq[String]
}

object StateDiagram {

  def apply[A](using diagram: StateDiagram[A]): StateDiagram[A] = diagram

  given state0Diagram[F[_], S, O](using
      ct: ClassTag[S]
  ): StateDiagram[State0Algebra[F, S, O]] =
    new StateDiagram[State0Algebra[F, S, O]] {
      override val diagram: Seq[String] = Vector(
        s"  ${ct.runtimeClass.getSimpleName} --> [*]"
      )
    }

  given state1Diagram[
      F[_],
      S0,
      E,
      S1,
      O1,
      A1 <: StateAlgebra[F, S1, O1]
  ](using
      ct0: ClassTag[S0],
      ct1: ClassTag[S1],
      diagram1: StateDiagram[A1]
  ): StateDiagram[State1Algebra[F, S0, E, S1, O1, A1]] =
    new StateDiagram[State1Algebra[F, S0, E, S1, O1, A1]] {
      override val diagram: Seq[String] = Vector(
        s"  ${ct0.runtimeClass.getSimpleName} --> ${ct1.runtimeClass.getSimpleName}"
      ) ++ diagram1.diagram
    }

  given state2Diagram[
      F[_],
      S0,
      E,
      S1,
      O1,
      S2,
      O2,
      A1 <: StateAlgebra[F, S1, O1],
      A2 <: StateAlgebra[F, S2, O2]
  ](using
      ct0: ClassTag[S0],
      ct1: ClassTag[S1],
      ct2: ClassTag[S2],
      diagram1: StateDiagram[A1],
      diagram2: StateDiagram[A2]
  ): StateDiagram[State2Algebra[F, S0, E, S1, O1, S2, O2, A1, A2]] =
    new StateDiagram[State2Algebra[F, S0, E, S1, O1, S2, O2, A1, A2]] {
      override val diagram: Seq[String] = Vector(
        s"  ${ct0.runtimeClass.getSimpleName} --> ${ct1.runtimeClass.getSimpleName}",
        s"  ${ct0.runtimeClass.getSimpleName} --> ${ct2.runtimeClass.getSimpleName}"
      ) ++ diagram1.diagram
        ++ diagram2.diagram
    }

  given stateProgramDiagram[F[_], S, O, A <: StateAlgebra[F, S, O]](using
      ct: ClassTag[S],
      diagramA: StateDiagram[A]
  ): StateDiagram[StateProgram[F, S, O, A]] =
    new StateDiagram[StateProgram[F, S, O, A]] {
      override val diagram: Seq[String] = Vector(
        "---",
        s"title: ${ct.runtimeClass.getSimpleName} FSM",
        "---",
        "stateDiagram-v2",
        s"  [*] --> ${ct.runtimeClass.getSimpleName}"
      ) ++ diagramA.diagram.distinct
    }

  given stateWithAlgebraDiagram[
      F[_],
      S,
      O,
      A <: StateAlgebra[F, S, O]
  ](using
      diagramA: StateDiagram[StateProgram[F, S, O, A]]
  ): StateDiagram[StateProgram.WithAlgebra[F, S, O, A]] =
    new StateDiagram[StateProgram.WithAlgebra[F, S, O, A]] {
      override val diagram: Seq[String] = diagramA.diagram
    }

}
