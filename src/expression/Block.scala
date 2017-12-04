package expression
import value._
case class Block(val locals: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    val localEnv = new Environment(env)
    val args = locals.map(_.execute(localEnv))
    args(args.length - 1)
  }
}