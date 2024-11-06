package com.github.alixba.fsm

import com.github.alixba.fsm.doc.StateDiagram
import com.github.alixba.fsm.framework.{
  StateAlgebra,
  StateAlgebraMerger,
  StateProgram
}

package object syntax {

  object framework {
    extension [F[_], S, O, A <: StateAlgebra[F, S, O]](_algebra: A) {
      def program(using
          program: StateProgram[F, S, O, A]
      ): StateProgram.WithAlgebra[F, S, O, A] =
        new StateProgram.WithAlgebra[F, S, O, A] {
          override def algebra: A = _algebra
          override def run(algebra: A)(s: S): F[O] = program.run(algebra)(s)
        }

      def merge[S2, O2, A2 <: StateAlgebra[F, S2, O2]](algebra2: A2)(using
          merger: StateAlgebraMerger[F, S, O, S2, O2, A, A2]
      ): merger.A3 = merger(_algebra, algebra2)
    }
  }

  object doc {
    extension [
        F[_],
        S,
        O,
        A <: StateAlgebra[F, S, O],
        P <: StateProgram.WithAlgebra[F, S, O, A]
    ](program: P) {
      def diagram(using diagram: StateDiagram[P]): Seq[String] =
        diagram.diagram
    }
  }

}
