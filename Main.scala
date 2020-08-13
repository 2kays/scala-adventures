package scala_adventures

import scala.collection.immutable.List
import scala.collection.mutable.ArrayStack

sealed trait Op
case class Push(item: Int) extends Op
case object Add extends Op
case object Pop extends Op
case object PeekNPrint extends Op

class Interpreter(val program: List[Op]) {
  var stack = new ArrayStack[Int]

  def run: Unit = run(program)

  def run(fragment: List[Op]): Unit = {
    val x::xs = fragment

    (x: Op) match {
      case Push(item) => stack.push(item)
      case Pop => stack.pop()
      case Add => stack.push(stack.pop() + stack.pop())
      case PeekNPrint => println(stack.head)
    }

    xs.headOption match {
      case Some(_) => run(xs)
      case None => return
    }
  }
}

object Main extends App {
  val testProgram = List[Op](
    Push(10),
    Push(20),
    Add,
    PeekNPrint,
    Pop
  )

  new Interpreter(testProgram).run
}
