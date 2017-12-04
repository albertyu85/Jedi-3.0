package system
import expression._
import value._
class TypeException(gripe: String = "Type Error") extends JediException(gripe)