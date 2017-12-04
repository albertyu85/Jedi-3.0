package value
import expression._
import system.UndefinedException
//import value._
import scala.collection.mutable.HashMap 
class Environment(var extension: Environment = null) extends HashMap[Identifier, Value] with Value {
  override def apply(name: Identifier): Value = {
    if (this.contains(name)) super.apply(name)
    else if (extension != null) extension.apply(name)
    else throw new UndefinedException(name)
  }
}
