package com.github.edgecaseberg.workflow

import org.scalatest._

class WorkflowTest extends FlatSpec with Matchers{

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

	"A log" should "be considered a Start State if empty" in {
		val result = Workflow.determineCurrentState(Nil, LoopWorkflow.workflow)
		assertResult(None) {
			result
		}
	}

	it should "resolve a simple one step transition" in {
		val log = List(
			LogEntry(LinearWorkflow.startState, LinearWorkflow.s1, "Null -> S1", Forward, LinearWorkflow.startAction)
		)
		assertResult(LinearWorkflow.s1) {
			Workflow.determineCurrentState(log, LinearWorkflow.workflow)
		}
	}

	it should "resolve a finished linear transition" in {
		val log = List(
			LogEntry(LinearWorkflow.startState, LinearWorkflow.s1, "Null -> S1", Forward, LinearWorkflow.startAction),
			LogEntry(LinearWorkflow.s1, LinearWorkflow.s2, "S1 -> S2", Forward, LinearWorkflow.a1)
		)
		assertResult(LinearWorkflow.s2) {
			Workflow.determineCurrentState(log, LinearWorkflow.workflow)
		}	
	}

}