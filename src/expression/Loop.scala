package expression
import value._
case class Loop(val statement: Expression, val body: Expression) extends SpecialForm{
  def execute(env: Environment): value.Value =  {
    val a = statement.execute(env).asInstanceOf[Number].value.toInt
    for(i <- 1 until a) {
      body.execute(env)
    }
    body.execute(env)
   Notification.DONE
  }
  
}