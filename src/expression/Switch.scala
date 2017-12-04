package expression
import value._
import system._
case class Switch(val ex: Expression, val block: Block) extends SpecialForm{
  def execute(env: Environment) : value.Value ={
    if (!ex.execute(env).isInstanceOf[Number]) 
      throw new TypeException("Switch selector must be a non-negative integer")
    
    val index = ex.execute(env).asInstanceOf[Number].value.toInt
    if (index > block.locals.size) throw new IndexOutOfBoundsException("UNDEFINED")
    block.locals(index).execute(env)
   
  }
  
}