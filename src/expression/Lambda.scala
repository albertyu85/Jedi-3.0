package expression
import value._
case class Lambda(val parameter: List[Identifier], val body: Expression) extends SpecialForm {
  def execute(env: Environment): Closure = {
   val a = new Closure(parameter, body, env)
  a
  }
}