package com.github.edgecaseberg.workflow

import scala.collection.immutable.Set
import scala.collection.mutable.Stack

sealed trait Direction
case object Forward extends Direction
case object Backward extends Direction

case class Action(from: State, to: State, flow: Direction, name: String)

case class State(name: String, preRequisite: Option[State] = None)

case class LogEntry(startState: State, endState: State, note: String, flowTaken: Direction, actionTaken: Action)

case class Workflow(states: List[State], actions: List[Action]) {
	def possibleActionsForState(state: State) = {
		Workflow.possibleActionsForState(state, this)
	}

	def possibleActionsForLog(log: Seq[LogEntry]) = {
		Workflow.possibleActionsForLog(log, this)
	}

	def determineCurrentState(log: Seq[LogEntry]) = {
		Workflow.determineCurrentState(log, this)
	}
}

object Workflow {
	/** Determine the current state of a tracked object by it's Log and the workflow it adheres to
	 *
	 */
	def determineCurrentState(log: Seq[LogEntry], workflow: Workflow): Set[State] = {
		var sequences = List[Stack[LogEntry]](
			Stack[LogEntry]()
		)
		if (log.isEmpty) {
			Set[State]()
		} else {
			for (entry <- log) {
				var pushed = false
				entry.flowTaken match {
					case Forward =>
						sequences.map { stack =>
							if (stack.isEmpty ||
								stack.headOption.map(_.endState) == Some(entry.startState)) {
								stack.push(entry)
								pushed = true
							}
						}
						if (!pushed) {
							val newStack = Stack[LogEntry](entry)
							sequences = sequences :+ newStack
						}
					case Backward =>
						sequences.map { stack =>
							stack.headOption.map { topEntry =>
								if (topEntry.endState == entry.startState && !pushed) {
									stack.push(entry)
									pushed = true
								}
							}
						}
				}
			}
		}
		sequences.map(_.headOption).filter(_.isDefined).map(_.get.endState).toSet
	}

	def possibleActionsForState(state: State, workflow: Workflow): List[Action] = {
		workflow.actions.filter(_.from == state)
	}

	def possibleActionsForLog(log: Seq[LogEntry], workflow: Workflow): Map[State, List[Action]] = {
		determineCurrentState(log, workflow).map(state => state -> possibleActionsForState(state, workflow)).toMap
	}
}
