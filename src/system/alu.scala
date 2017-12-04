package system

import expression._
import system._
import value._
import value.Boole
import value.Number
import value.Value
object alu {
	//dispatcher
	def execute (operator: Identifier, args: List[Value]): Value = {
			operator.name match {
			case "add" => add(args)
			case "mul" => mul(args)
			case "div" => div(args)
			case "sub" => sub(args)
			case "equals" => equals(args)
			case "unequals" => unequals(args)
			case "less" => less(args)
			case "more" => more(args)
			case "not" => not(args)
			case "write" => write(args)
			case "prompt" => prompt(args)
			case "read" => read(args)
			case "content" => content(args)
			case "var" => makeVar(args)
			case _ => throw new UndefinedException(operator)

			}
	}

	private def castAsNumbers(vals: List[Value], opcode: String): List[Number] = {
			if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
			val ok = vals.filter(_.isInstanceOf[Number])
			if (ok.length < vals.length) throw new TypeException(opcode + " inputs must be numbers")
			vals.map(_.asInstanceOf[Number])
	}

	private def add(args: List[Value]): Number = {
			var nums = args.filter(_.isInstanceOf[Number])
					if (nums.length != args.length)
						throw new TypeException("Inputs to add must be numbers")
						val nums2 = nums.map(_.asInstanceOf[Number])
						nums2.reduce(_+_)
	}
	private def mul(args: List[Value]): Number = {
			var nums = args.filter(_.isInstanceOf[Number])
					if (nums.length != args.length)
						throw new TypeException("Inputs to add must be numbers")
						val nums2 = nums.map(_.asInstanceOf[Number])
						nums2.reduce(_*_)

	}
	private def div(args: List[Value]): Number = {
			var nums = args.filter(_.isInstanceOf[Number])
					if (nums.length != args.length)
						throw new TypeException("Inputs to add must be numbers")
						val nums2 = nums.map(_.asInstanceOf[Number])
						nums2.reduce(_/_)

	}
	private def sub(args: List[Value]): Number = {
			var nums = args.filter(_.isInstanceOf[Number])
					if (nums.length != args.length)
						throw new TypeException("Inputs to add must be numbers")
						val nums2 = nums.map(_.asInstanceOf[Number])
						nums2.reduce(_-_)

	}
	private def equals(args: List[Value]): Boole = {
			var nums = args.filter(_.isInstanceOf[Number])
					if (nums.length != args.length)
						throw new TypeException("Inputs to add must be numbers")
						val nums2 = nums.map(_.asInstanceOf[Number])
						if (nums2(0) == nums2(1)) 
							Boole(true)
							else
								Boole(false)   
	}
	private def unequals(args: List[Value]): Boole = {
			var nums = args.filter(_.isInstanceOf[Number])
					if (nums.length != args.length)
						throw new TypeException("Inputs to add must be numbers")
						val nums2 = nums.map(_.asInstanceOf[Number])
						if (nums2(0) != nums2(1)) 
							Boole(true)
							else
								Boole(false)   
	}
	private def less(args: List[Value]): Boole = {
			var nums = args.filter(_.isInstanceOf[Number])
					if (nums.length != args.length)
						throw new TypeException("Inputs to add must be numbers")
						val nums2 = nums.map(_.asInstanceOf[Number])
						if (nums2(0).value < nums2(1).value) 
							Boole(true)
							else
								Boole(false)   
	} 

	def more(vals: List[Value]): Value = {
			val args = castAsNumbers(vals, "more")
					if (args.length != 2)  throw new TypeException("more inputs must be numbers")
					if (args(0).value > args(1).value) Boole(true) else Boole(false)
	}
	def not(vals: List[Value]): Value = {
			if (vals.length != 1) throw new TypeException("not expected 1 input")
			if (!vals(0).isInstanceOf[Boole]) throw new TypeException("input to not must be Boole")
			(vals(0).asInstanceOf[Boole]).not // can't get Boole.! to work
	}
	def content(args: List[Value]): Value = {
	  if (!(args.head.isInstanceOf[Variable])) 
	    throw new TypeException("Must be of type Value")
	  args.head.asInstanceOf[Variable].content
	}
	def makeVar(args: List[Value]): Variable = {
	  new Variable(args.head)
	}
	def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
	def read(vals: List[Value]): Value = { val result = readDouble(); Number(result)}
	def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }


}