package expression
import value._
case class Declaration(val name: Identifier, val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = { 
    val result = body.execute(env)
    env.put(name, result)
    Notification.OK
  }
}