package com.github.edgecaseberg.workflow

import org.scalatest._
import scala.collection.immutable.Set

class WorkflowTest extends FlatSpec with Matchers {

	object LoopWorkflow {
		val startState = State("Start", None)
		val s1 = State("S1", Some(startState))
		val s2 = State("S2", Some(s1))
		val s3 = State("S3", Some(s2))

		val startAction = Action(startState, s1, Forward, "a0")
		val a1 = Action(s1, s2, Forward, "a1")
		val a2 = Action(s2, s1, Backward, "a2")
		val a3 = Action(s2, s3, Forward, "a3")

		val workflow = Workflow(
			states = List(
				startState, s1, s2, s3
			),
			actions = List(
				startAction, a1, a2, a3
			)
		)
	}

	object LinearWorkflow {
		val startState = State("Start", None)
		val s1 = State("S1", Some(startState))
		val s2 = State("S2", Some(s1))

		val startAction = Action(startState, s1, Forward, "a0")
		val a1 = Action(s1, s2, Forward, "a1")

		val workflow = Workflow(
			states = List(startState, s1, s2),
			actions = List(startAction, a1)
		)
	}

	object MultiEndStateLinearWorflow {
		val startState = State("Start", None)
		val s1 = State("S1", Some(startState))
		val s2 = State("S2", Some(s1))
		val s3 = State("S3", Some(s1))
		val s4 = State("S4", Some(s2))
		val s5 = State("S5", Some(s3))

		val startAction = Action(startState, s1, Forward, "a0")
		val a1 = Action(s1, s2, Forward, "a1")
		val a2 = Action(s2, s4, Forward, "a2")
		val a3 = Action(s1, s3, Forward, "a3")
		val a4 = Action(s3, s5, Forward, "a4")

		val workflow = Workflow(
			states = List(startState, s1, s2, s3, s4, s5),
			actions = List(startAction, a1, a2, a3, a4)
		)
	}

	object MultiEndLoopWorkflow {
		val startState = State("Start", None)
		val s1 = State("S1", Some(startState))
		val s2 = State("S2", Some(s1))
		val s3 = State("S3", Some(s2))
		val s4 = State("S4", Some(s2))

		val startAction = Action(startState, s1, Forward, "a0")
		val a1 = Action(s1, s2, Forward, "a1")
		val a2 = Action(s2, s1, Backward, "a2")
		val a3 = Action(s2, s3, Forward, "a3")
		val a4 = Action(s3, s4, Forward, "a4")

		val workflow = Workflow(
			states = List(
				startState, s1, s2, s3, s4
			),
			actions = List(
				startAction, a1, a2, a3, a4
			)
		)
	}

	"A log" should "be considered a Start State if empty" in {
		val result = Workflow.determineCurrentState(Nil, LoopWorkflow.workflow)
		assertResult(Set[State]()) {
			result
		}
	}

	"A call to determineCurrentState" should "resolve a simple one step transition" in {
		val log = List(
			LogEntry(LinearWorkflow.startState, LinearWorkflow.s1, "Null -> S1", Forward, LinearWorkflow.startAction)
		)
		assertResult(Set(LinearWorkflow.s1)) {
			Workflow.determineCurrentState(log, LinearWorkflow.workflow)
		}
	}

	it should "resolve a finished linear transition" in {
		val log = List(
			LogEntry(LinearWorkflow.startState, LinearWorkflow.s1, "Null -> S1", Forward, LinearWorkflow.startAction),
			LogEntry(LinearWorkflow.s1, LinearWorkflow.s2, "S1 -> S2", Forward, LinearWorkflow.a1)
		)
		assertResult(Set(LinearWorkflow.s2)) {
			Workflow.determineCurrentState(log, LinearWorkflow.workflow)
		}
	}

	it should "resolve a loop transition" in {
		val log = List(
			LogEntry(LoopWorkflow.startState, LoopWorkflow.s1, "Null -> S1", Forward, LoopWorkflow.startAction),
			LogEntry(LoopWorkflow.s1, LoopWorkflow.s2, "S1 -> S2", Forward, LoopWorkflow.a1),
			LogEntry(LoopWorkflow.s2, LoopWorkflow.s1, "S2 -> S1", Backward, LoopWorkflow.a2),
			LogEntry(LoopWorkflow.s1, LoopWorkflow.s2, "S1 -> S2", Forward, LoopWorkflow.a1),
			LogEntry(LoopWorkflow.s2, LoopWorkflow.s1, "S2 -> S1", Backward, LoopWorkflow.a2),
			LogEntry(LoopWorkflow.s1, LoopWorkflow.s2, "S1 -> S2", Forward, LoopWorkflow.a1),
			LogEntry(LoopWorkflow.s2, LoopWorkflow.s3, "S2 -> S3", Forward, LoopWorkflow.a3)
		)
		assertResult(Set(LoopWorkflow.s3)) {
			Workflow.determineCurrentState(log, LoopWorkflow.workflow)
		}
	}

	it should "resolve a multi-end state linear transition" in {
		val log = List(
			LogEntry(MultiEndStateLinearWorflow.startState, MultiEndStateLinearWorflow.s1, "∅ → S1", Forward, MultiEndStateLinearWorflow.startAction),
			LogEntry(MultiEndStateLinearWorflow.s1, MultiEndStateLinearWorflow.s2, "S1 → S2", Forward, MultiEndStateLinearWorflow.a1),
			LogEntry(MultiEndStateLinearWorflow.s1, MultiEndStateLinearWorflow.s3, "S1 → S3", Forward, MultiEndStateLinearWorflow.a3),
			LogEntry(MultiEndStateLinearWorflow.s2, MultiEndStateLinearWorflow.s4, "S2 → S4", Forward, MultiEndStateLinearWorflow.a2),
			LogEntry(MultiEndStateLinearWorflow.s3, MultiEndStateLinearWorflow.s5, "S3 → S5", Forward, MultiEndStateLinearWorflow.a4)
		)
		assertResult(Set(MultiEndStateLinearWorflow.s4, MultiEndStateLinearWorflow.s5)) {
			Workflow.determineCurrentState(log, MultiEndStateLinearWorflow.workflow)
		}
	}

	it should "resolve a multi-end state looping transition" in {
		val log = List(
			LogEntry(MultiEndLoopWorkflow.startState, MultiEndLoopWorkflow.s1, "Null -> S1", Forward, MultiEndLoopWorkflow.startAction),
			LogEntry(MultiEndLoopWorkflow.s1, MultiEndLoopWorkflow.s2, "S1 -> S2", Forward, MultiEndLoopWorkflow.a1),
			LogEntry(MultiEndLoopWorkflow.s2, MultiEndLoopWorkflow.s1, "S2 -> S1", Backward, MultiEndLoopWorkflow.a2),
			LogEntry(MultiEndLoopWorkflow.s1, MultiEndLoopWorkflow.s2, "S1 -> S2", Forward, MultiEndLoopWorkflow.a1),
			LogEntry(MultiEndLoopWorkflow.s2, MultiEndLoopWorkflow.s1, "S2 -> S1", Backward, MultiEndLoopWorkflow.a2),
			LogEntry(MultiEndLoopWorkflow.s1, MultiEndLoopWorkflow.s2, "S1 -> S2", Forward, MultiEndLoopWorkflow.a1),
			LogEntry(MultiEndLoopWorkflow.s2, MultiEndLoopWorkflow.s3, "S2 -> S3", Forward, MultiEndLoopWorkflow.a3),
			LogEntry(MultiEndLoopWorkflow.s2, MultiEndLoopWorkflow.s4, "S2 -> S4", Forward, MultiEndLoopWorkflow.a4)
		)
		assertResult(Set(MultiEndLoopWorkflow.s3, MultiEndLoopWorkflow.s4)) {
			Workflow.determineCurrentState(log, MultiEndLoopWorkflow.workflow)
		}
	}

	"The Workflow object" should "determine possible actions for a state" in {
		assertResult(List(MultiEndLoopWorkflow.a2, MultiEndLoopWorkflow.a3)) {
			MultiEndLoopWorkflow.workflow.possibleActionsForState(MultiEndLoopWorkflow.s2)
		}
		assertResult(List()) {
			LinearWorkflow.workflow.possibleActionsForState(LinearWorkflow.s2)
		}
	}

	it should "determine actions given a log of events" in {
		val log = List(
			LogEntry(MultiEndLoopWorkflow.startState, MultiEndLoopWorkflow.s1, "Null -> S1", Forward, MultiEndLoopWorkflow.startAction),
			LogEntry(MultiEndLoopWorkflow.s1, MultiEndLoopWorkflow.s2, "S1 -> S2", Forward, MultiEndLoopWorkflow.a1),
			LogEntry(MultiEndLoopWorkflow.s2, MultiEndLoopWorkflow.s1, "S2 -> S1", Backward, MultiEndLoopWorkflow.a2),
			LogEntry(MultiEndLoopWorkflow.s1, MultiEndLoopWorkflow.s2, "S1 -> S2", Forward, MultiEndLoopWorkflow.a1),
			LogEntry(MultiEndLoopWorkflow.s2, MultiEndLoopWorkflow.s1, "S2 -> S1", Backward, MultiEndLoopWorkflow.a2),
			LogEntry(MultiEndLoopWorkflow.s1, MultiEndLoopWorkflow.s2, "S1 -> S2", Forward, MultiEndLoopWorkflow.a1)
		)
		assertResult(
			Map(
				MultiEndLoopWorkflow.s2 -> List(
					MultiEndLoopWorkflow.a2, MultiEndLoopWorkflow.a3
				)
			)
		) {
				MultiEndLoopWorkflow.workflow.possibleActionsForLog(log)
			}
	}

	it should "determine actions given a log of events with multi-end" in {
		val log = List(
			LogEntry(MultiEndLoopWorkflow.startState, MultiEndLoopWorkflow.s1, "Null -> S1", Forward, MultiEndLoopWorkflow.startAction),
			LogEntry(MultiEndLoopWorkflow.s1, MultiEndLoopWorkflow.s2, "S1 -> S2", Forward, MultiEndLoopWorkflow.a1),
			LogEntry(MultiEndLoopWorkflow.s2, MultiEndLoopWorkflow.s1, "S2 -> S1", Backward, MultiEndLoopWorkflow.a2),
			LogEntry(MultiEndLoopWorkflow.s1, MultiEndLoopWorkflow.s2, "S1 -> S2", Forward, MultiEndLoopWorkflow.a1),
			LogEntry(MultiEndLoopWorkflow.s2, MultiEndLoopWorkflow.s1, "S2 -> S1", Backward, MultiEndLoopWorkflow.a2),
			LogEntry(MultiEndLoopWorkflow.s1, MultiEndLoopWorkflow.s2, "S1 -> S2", Forward, MultiEndLoopWorkflow.a1),
			LogEntry(MultiEndLoopWorkflow.startState, MultiEndLoopWorkflow.s1, "Null -> S1", Forward, MultiEndLoopWorkflow.startAction),
			LogEntry(MultiEndLoopWorkflow.s1, MultiEndLoopWorkflow.s2, "S1 -> S2", Forward, MultiEndLoopWorkflow.a1),
			LogEntry(MultiEndLoopWorkflow.s2, MultiEndLoopWorkflow.s1, "S2 -> S1", Backward, MultiEndLoopWorkflow.a2)
		)
		assertResult(
			Map(
				MultiEndLoopWorkflow.s2 -> List(
					MultiEndLoopWorkflow.a2, MultiEndLoopWorkflow.a3
				),
				MultiEndLoopWorkflow.s1 -> List(
					MultiEndLoopWorkflow.a1
				)
			)
		) {
				MultiEndLoopWorkflow.workflow.possibleActionsForLog(log)
			}
	}

}