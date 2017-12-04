package expression
import value._
import system._
case class Iteration(val condition: Expression, val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    if (!(condition.execute(env).isInstanceOf[Boole])) {
      throw new TypeException("Must be of Type Boole")
    }
    var cond = condition.execute(env).asInstanceOf[Boole]
   
    while(cond.value) {
      body.execute(env)
     
     cond = condition.execute(env).asInstanceOf[Boole]
    
    }
    Notification.DONE
    
  }
}