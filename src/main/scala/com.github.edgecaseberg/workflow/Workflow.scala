package com.github.edgecaseberg.workflow

sealed trait Direction
case object Forward extends Direction
case object Backward extends Direction

case class Action(from: State, to: State, flow: Direction, name: String)

case class State(name: String, preRequisite: Option[State] = None)

case class LogEntry(startState: State, endState: State, note: String, flowTaken: Direction, actionTake: Action)

case class Workflow(states: List[State], actions: List[Action])

object Workflow {
	def determineCurrentState(log: Seq[LogEntry], workflow: Workflow) : Option[State] = {
		if (log.isEmpty) {
			None
		} else {
			None //TODO
		}
	}
}
