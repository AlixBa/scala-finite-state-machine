package com.github.alixba.fsm.doc

import com.github.alixba.fsm.TestFSM.*
import com.github.alixba.fsm.framework.StateAlgebra.{
  State0Algebra,
  State1Algebra,
  State2Algebra
}
import com.github.alixba.fsm.framework.StateProgram
import com.github.alixba.fsm.syntax.framework.program
import com.github.alixba.fsm.syntax.doc.diagram
import munit.FunSuite

import scala.util.Try

class StateDiagramTest extends FunSuite {
  test("generate mermaid state diagram") {
    assertEquals(
      fsm().program.diagram,
      Seq(
        "---",
        "title: InitialState FSM",
        "---",
        "stateDiagram-v2",
        "  [*] --> InitialState",
        "  InitialState --> RetrievingAccount",
        "  RetrievingAccount --> Rejected",
        "  RetrievingAccount --> HoldingFunds",
        "  Rejected --> [*]",
        "  HoldingFunds --> Rejected",
        "  HoldingFunds --> Accepted",
        "  Accepted --> [*]"
      )
    )
  }
}
