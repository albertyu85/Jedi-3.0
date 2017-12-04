package system
import scala.util.parsing.combinator._
import expression._
import value._

class SithParsers extends RegexParsers {

	def expression: Parser[Expression] = declaration | conditional | switch | iteration | assignment | disjunction | failure("Invalid expression")
	def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
			case "def"~id~"="~exp => Declaration(id, exp)
	}
	def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
     case "if"~"("~cond~")"~cons~ None => Conditional(cond, cons)
     case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
   }
	   def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
     case eq ~ Nil => eq
     case eq ~ more => Conjunction(eq:: more)
   }

	def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
		case con ~ Nil => con
		case con ~ cons => Disjunction(con::cons)
			}
	def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
     case ineq~Nil => ineq
     case ineq~more => FunCall(Identifier("equals"), ineq::more)
   }

			
  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
    case t~None=> t
    case t~Some("<" ~ s)=> FunCall(Identifier("less"), List(t, s))
    case t~Some(">" ~ s)=> FunCall(Identifier("more"), List(t, s))
    case t~Some("!=" ~ s)=> FunCall(Identifier("unequals"), List(t, s))
  }

	def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }
    

def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product ^^ {
    case "+"~s=>s
    case "-"~s=> negate(s)
    })^^{
    case p~Nil=> p
    case p~rest=>FunCall(Identifier("add"), p::rest)
    }

 def invert(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1)
    FunCall(div, List(one, exp))
  }
 

def product: Parser[Expression] = term ~ rep(("*"|"/") ~ term ^^ {
    case "*"~s=>s
    case "/"~s=>invert(s)
    })^^{
    case p~Nil=> p
    case p~rest=>FunCall(Identifier("mul"), p::rest)
    }
   

		
def term: Parser[Expression]  = loop |deref | assignment | lambda  | block | funCall | literal |  "("~>expression<~")"
def literal =  boole | numeral | identifier

def boole: Parser[Boole] = ("true" | "false") ^^ {
     case "true" => Boole(true)
     case "false" => Boole(false)
   }
def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep(","~>expression))<~")" ^^ {
     case None => Nil
     case Some(e ~ Nil) => List(e)
     case Some(e ~ exps) => e :: exps
     case _ => Nil
   }

def funCall: Parser[Expression] = identifier~operands ^^ {
     case op~ops => FunCall(op, ops)
     case Identifier("var") ~ ops => FunCall(Identifier("makeVar"), ops)
   }
def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
     case someString => Identifier(someString)
   }

 def numeral: Parser[Number] = """-?\d+(\d+)*(\.\d+(e\d+)?)?""".r ^^ {
     case digits => Number(digits.toDouble)
   }

 def block: Parser[Block] ="{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
  case "{" ~ eq ~ Nil ~ "}" => Block(List[Expression](eq))
  case "{" ~ eq ~ eqs ~ "}" => Block(eq :: eqs)
}
  
 def lambda: Parser[Lambda] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ param ~ exp => Lambda(param, exp)
  }
def parameters: Parser[List[Identifier]] =  "(" ~> opt(identifier ~ rep(","~>identifier))<~")" ^^
{
  case None => Nil 
  case Some(e ~ Nil) => List(e) 
  case Some(e ~ exps) => e::exps 
  case _ => Nil
}

def iteration: Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression^^ {
  case "while" ~ "(" ~ exp ~ ")" ~ exps => Iteration(exp, exps)
}
def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^ {
  case id ~ "=" ~ ex => Assignment(id, ex)
}
def deref: Parser[Expression] = "[" ~>expression<~"]" ^^ {
  case exp => {
    FunCall(Identifier("content"), List[Expression](exp))
    
  }
}
def loop: Parser[Loop] = "loop" ~"[" ~ expression ~ "]" ~ expression ^^ {
  case "loop" ~ "[" ~ ex ~ "]" ~ exp => Loop(ex, exp)
}
def switch = "switch" ~ "(" ~ expression ~ ")" ~ block ^^ {
case "switch" ~ "(" ~ ex ~ ")" ~ bl => Switch(ex, bl)
  }
}

// def declaration, conditional, dusjunction, and other parsers
