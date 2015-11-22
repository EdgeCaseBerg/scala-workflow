package com.github.edgecaseberg.workflow

import scala.collection.immutable.Set
import scala.collection.mutable.Stack

sealed trait Direction
case object Forward extends Direction
case object Backward extends Direction

case class Action(from: State, to: State, flow: Direction, name: String)

case class State(name: String, preRequisite: Option[State] = None)

case class LogEntry(startState: State, endState: State, note: String, flowTaken: Direction, actionTake: Action)

case class Workflow(states: List[State], actions: List[Action])

object Workflow {
	/** Determine the current state of a tracked object by it's Log and the workflow it adheres to
	 *
	 */
	def determineCurrentState(log: Seq[LogEntry], workflow: Workflow) : Set[State] = {
		val stack = new Stack[LogEntry]
		if (log.isEmpty) {
			Set[State]()
		} else {
			for (entry <- log) {
				entry.flowTaken match {
					case Forward => stack.push(entry)
					case Backward => stack.pop(); stack.push(entry)
				}
			}
		}
		println(stack)
		if ( stack.isEmpty ) {
			Set[State]()
		} else {
			Set(stack.pop().endState)
		}
	}
}
