package expression
import value._
import system._
case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  def execute(env: Environment): Value = {
    val args = operands.map(_.execute(env))
    try {
      val fun = operator.execute(env)
      if (!(fun.isInstanceOf[Closure]))
        throw new TypeException("Error")
      fun.asInstanceOf[Closure].apply(args)
        
    } catch {
      case undefinedException => alu.execute(operator, args)
    }
  
   
  }
}

/*case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression{
  def execute(env: Environment): Value = {
    var args = List[Value]()
    for (exp <- operands) {
    args = args:::args
    }
    
    alu.execute(operator, args)
  }
  
}*/