package value
import expression._
case class Variable(var content: Value) extends expression.Literal {
 
override def toString() = "Variable(" + content.toString() + ")"
}