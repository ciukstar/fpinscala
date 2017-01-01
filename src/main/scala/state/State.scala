package state

case class State[S,+A](run: S => (A,S))

object State
