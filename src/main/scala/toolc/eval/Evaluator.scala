package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    val ectx = new MainContext
    prog.main.stats.foreach(evalStatement(_)(ectx))
  }

  def evalStatement(stmt: StatTree)(implicit ectx: EvaluationContext): Unit = stmt match {
    
    case Block(stats) =>
      stats.foreach { stat => evalStatement(stat) }

    case If(expr, thn, els) =>
      if (evalExpr(expr).asBool) {
        evalStatement(thn)
      } else {
        els match {
          case Some(s) => evalStatement(s)
          case None => 
        }
      }

    case While(expr, stat) =>
      while (evalExpr(expr).asBool) {
        evalStatement(stat)
      }

    case Println(expr) =>
      evalExpr(expr) match {
        
        case StringValue(i) => println(i)
        case IntValue(i) => println(i)
        case BoolValue(i) => println(i)
        case _ => fatal("Try to print an invalid type")
      }

    case Assign(id, expr) =>
      ectx.setVariable(id.value, evalExpr(expr))

    case ArrayAssign(id, index, expr) =>
      val interArray = ectx.getVariable(id.value).asArray
      interArray.setIndex((evalExpr(index).asInt), (evalExpr(expr).asInt))
      ectx.setVariable(id.value, interArray)

    case DoExpr(expr) => 
      evalExpr(expr)
  }

  def evalExpr(e: ExprTree)(implicit ectx: EvaluationContext): Value = e match {
    case IntLit(value)    => IntValue(value)

    case StringLit(value) => StringValue(value)

    case True()           => BoolValue(true)

    case False()          => BoolValue(false)

    case And(lhs, rhs) =>
      evalExpr(lhs) match {
        case (BoolValue(i1)) =>
          if (!i1) {
            BoolValue(false)
          } else {
            evalExpr(rhs) match {
              case (BoolValue(i2)) =>
                BoolValue(i1 && i2)
              case _ => fatal("And between Boolean only")
            }
          }
        case _ => fatal("And between Boolean only")
      }

    case Or(lhs, rhs) =>
      evalExpr(lhs) match {
        case (BoolValue(i1)) =>
          if (i1) {
            BoolValue(true)
          } else {
            evalExpr(rhs) match {
              case (BoolValue(i2)) =>
                BoolValue(i1 || i2)
              case _ => fatal("Or between Boolean only")
            }
          }
        case _ => fatal("Or between Boolean only")
      }

    case Plus(lhs, rhs) =>
       (evalExpr(lhs), evalExpr(rhs)) match {
        case (IntValue(i1), IntValue(i2)) =>
          IntValue(i1 + i2)
        case (StringValue(i1), StringValue(i2)) =>
          StringValue(i1 + i2)
        case (IntValue(i1), StringValue(i2)) =>
          StringValue(i1 + i2)
        case (StringValue(i1), IntValue(i2)) =>
          StringValue(i1 + i2)
        case _ => fatal("Plus between Int and String only")

      }

    case Minus(lhs, rhs) =>
       (evalExpr(lhs), evalExpr(rhs)) match {
        case (IntValue(i1), IntValue(i2)) =>
          IntValue(i1 - i2)
        case _ => fatal("Minus between Int only")
      }

    case Times(lhs, rhs) =>
       (evalExpr(lhs), evalExpr(rhs)) match {
        case (IntValue(i1), IntValue(i2)) =>
          IntValue(i1 * i2)
        case _ => fatal("Times between Int only")
      }

    case Div(lhs, rhs) =>
       (evalExpr(lhs), evalExpr(rhs)) match {
        case (IntValue(i1), IntValue(i2)) =>
          if (i2 == 0) {
            fatal("Division by 0")
          } else {
            IntValue(i1 / i2)
          }
        case _ => fatal("Divison between Int only")
      }

    case LessThan(lhs, rhs) =>
       (evalExpr(lhs), evalExpr(rhs)) match {
        case (IntValue(i1), IntValue(i2)) =>
          BoolValue(i1.<(i2))
        case _ => fatal("LessThan between Int only")
      }

    case Not(expr) =>
      evalExpr(expr) match {
        case BoolValue(i) =>
          BoolValue(!i)
        case _ => fatal("Not only for Boolean")
      }

    case Equals(lhs, rhs) =>
      
      (evalExpr(lhs), evalExpr(rhs)) match {
        case (IntValue(i1), IntValue(i2)) =>
          BoolValue(i1 == i2)
        case (i1 : ArrayValue, i2 : ArrayValue) =>
          BoolValue(i1 eq i2)
        case (BoolValue(i1), BoolValue(i2)) =>
          BoolValue(i1 == i2)
        case (i1 : StringValue, i2 : StringValue) =>
          BoolValue(i1 eq i2)
        case (i1 : ObjectValue, i2 : ObjectValue) =>
          BoolValue(i1 eq i2)
        case _ => fatal("Equals two variables with different type")
      }
      
    case ArrayRead(arr, index) =>
      evalExpr(arr) match {
        case i : ArrayValue => IntValue(i.getIndex(evalExpr(index).asInt))
        case _ => fatal("try to read index from a non-array type")
      }

    case ArrayLength(arr) =>
      evalExpr(arr) match {
        case i : ArrayValue => IntValue(i.length)
        case _ => fatal("access length of array from a non-array type")
      }

    case MethodCall(obj, meth, args) => {   
    
      val evalObj = evalExpr(obj).asObject
      val methCont = new MethodContext(evalObj)
      
      val methodInfo = findMethod(evalObj.cd, meth.value)
      val evalArgs = args.map { arg => evalExpr(arg) }
      val argsName = methodInfo.args
      
      assert(evalArgs.size == argsName.size)
           
      argsName.foreach { name => methCont.declareVariable(name.id.value) }
      argsName.zip(evalArgs).foreach(t => methCont.setVariable(t._1.id.value, t._2))
      
      methodInfo.vars.foreach { x => methCont.declareVariable(x.id.value) }
      
      methodInfo.stats.foreach { stat => evalStatement(stat)(methCont) }

      methodInfo.retType match {
        
        case StringType() => 
          StringValue((evalExpr(methodInfo.retExpr)(methCont)).asString)
        
        case IntType() =>
          IntValue((evalExpr(methodInfo.retExpr)(methCont)).asInt)
          
        case BooleanType() =>
          BoolValue((evalExpr(methodInfo.retExpr)(methCont)).asBool)

        case IntArrayType() =>
          (evalExpr(methodInfo.retExpr)(methCont)).asArray
          
        case ClassType(_, _) => // change
          (evalExpr(methodInfo.retExpr)(methCont)).asObject

      }
    }
     
    case Variable(Identifier(name)) =>
      ectx.getVariable(name)
      
    case New(tpe, _) =>
      val c = findClass(tpe.value)
      val o = ObjectValue(c)
      fieldsOfClass(c).foreach { name => o.declareField(name) }
      o

    case This() =>
      ectx match {
        case ctx : MethodContext => ctx.obj
        case _ => fatal("Access to this from the main")
      }
      
    case NewIntArray(size) =>
      ArrayValue(new Array[Int](evalExpr(size).asInt))
  
  }

  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
  }

  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '" + name + "'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
      if (vars contains name) {
        vars += name -> Some(v)
      } else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
      vars += name -> None
    }
  }

  class MainContext extends EvaluationContext {
    private def unavailable = fatal("The main object contains no variables and/or fields")
    def getVariable(name: String): Value = unavailable
    def setVariable(name: String, v: Value): Unit = unavailable
    def declareVariable(name: String): Unit = unavailable
  }

  def findMethod(cd: ClassDecl, name: String): MethodDecl = {    
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.map(p => findMethod(findClass(p.id.value), name))).getOrElse(fatal("Unknown method " + cd.id + "." + name))

  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '" + name + "'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.id.value))).getOrElse(Set())
  }

  sealed abstract class Value {
    private def expected(tp: String) = fatal(s"Unexpected value: found $this, expected $tp")

    def asInt: Int = expected("Int")
    def asString: String = expected("String")
    def asBool: Boolean = expected("Boolean")
    def asObject: ObjectValue = expected("Object")
    def asArray: ArrayValue = expected("Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal(s"Unknown field '$name'")
      }
    }

    def getField(name: String) = {
      fields.get(name) match {
        case Some(Some(v)) => v
        case Some(None)    => fatal(s"Field '$name' has not been initialized")
        case None          => fatal(s"Unknown field '$name'")
      }
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(entries: Array[Int]) extends Value {
    val length = entries.length

    private def checkBounds(index: Int) = {
      if (index < 0 || index >= length) {
        fatal(s"Index '$index' out of bounds (0 .. ${length - 1})")
      }
    }

    def setIndex(i: Int, v: Int) {
      checkBounds(i)
      entries(i) = v
    }

    def getIndex(i: Int) = {
      checkBounds(i)
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(v: String) extends Value {    
    override def asString = v
  }

  case class IntValue(v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(v: Boolean) extends Value {
    override def asBool = v
  }
}
