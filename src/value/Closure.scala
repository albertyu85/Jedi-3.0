package value
import expression._
class Closure (val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    val localEnv = new Environment(defEnv)
     for (i <- 0 until params.length) {
        localEnv.put(params(i), args(i))
    }
    body.execute(localEnv)
  }
}