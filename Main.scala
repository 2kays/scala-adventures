package scala_adventures

import scala.collection.immutable.List
import scala.collection.mutable.ArrayStack

class InterpreterStack extends ArrayStack[Int]

trait Op {
  def iapply(stack: InterpreterStack)
}

sealed abstract class ArithmeticBinOp(f: (Int, Int) => Int) extends Op {
  def iapply(stack: InterpreterStack) = stack.combine(f)
}

final case object Add extends ArithmeticBinOp(_ + _)
final case object Sub extends ArithmeticBinOp(_ - _)
final case object Mul extends ArithmeticBinOp(_ * _)
final case object Div extends ArithmeticBinOp(_ / _)
final case object Mod extends ArithmeticBinOp(_ % _)
// final case object Pow extends ArithmeticBinOp(Math.pow(_, _))

final case class Push(item: Int) extends Op {
  def iapply(stack: InterpreterStack) = stack.push(item)
}

final case object Pop extends Op {
  def iapply(stack: InterpreterStack) = stack.pop()
}

final case object PeekNPrint extends Op {
  def iapply(stack: InterpreterStack) = println(stack.head)
}

final case object Dup extends Op {
  def iapply(stack: InterpreterStack) = stack.dup()
}

class Interpreter(val program: List[Op]) {
  var stack = new InterpreterStack

  def run: Unit = run(program)

  def run(fragment: List[Op], counter: Int = 0): Unit = {
    val x::xs = fragment

    try {
      x.iapply(stack)
    } catch {
      case (e: Throwable) => {
        val ename = e.getClass.getSimpleName
        println(s"Error L${counter}: ${ename}")
        return
      }
    }

    xs.headOption match {
      case Some(_) => run(xs, counter + 1)
      case None => return
    }
  }
}

object Main extends App {
  val testProgram = List[Op](
    Push(10),
    Push(20),
    Dup,
    Add,
    Sub,
    PeekNPrint,
    Pop
  )

  new Interpreter(testProgram).run
}
