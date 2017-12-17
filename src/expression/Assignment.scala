//add test comment
package expression
import value._
import system._
case class Assignment( val vbl: Identifier, val update: Expression) extends SpecialForm {
 
  def execute(env: Environment): Value = {
    
    val e = vbl.execute(env)
    if (e.isInstanceOf[Variable])
      
     e.asInstanceOf[Variable].content = update.execute(env)
     
     Notification.DONE
  }
}