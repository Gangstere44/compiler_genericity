package toolc
package analyzer

import ast.Trees._

import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /**
   * Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages.
   */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcClass(klass: ClassDecl): Unit = klass.methods.foreach(tcMethod)

    /** Type checks statements and return expression of the method */
    def tcMethod(meth: MethodDecl): Unit = {
      meth.stats.foreach { s => tcStat(s) }
      tcExpr(meth.retExpr, meth.retType.getType)
      (meth.args.reverse zip meth.getSymbol.argList).foreach { z =>
        if (!z._1.getSymbol.getType.isSubTypeOf(z._2.getType)) {
          error("Type error: Expected: " + z._2.getType.toString() + " OR one of its subtype, and found : " + z._1.getSymbol.getType.toString(), z._1)
        }
      }
    }

    /**
     * Checks that the expression is a subtype of the ones in expectedTps.
     * If it's not, prints an error message and returns the error type.
     * Also adds missing symbols to methods in MethodCalls
     */
    def tcExpr(expr: ExprTree, expectedTps: Type*): Unit = {
      expr match {
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        case Not(e) =>
          tcExpr(e, TBoolean)
        case Plus(lhs, rhs) =>
          tcExpr(lhs, TString, TInt)
          tcExpr(rhs, TString, TInt)
        case Minus(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Times(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Div(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Equals(lhs, rhs) =>
          lhs.getType match {
            case TClass(_, _) => { // TODO
              tcExpr(lhs, lhs.getType)
              tcExpr(rhs, rhs.getType)
            }
            case TInt => {
              tcExpr(lhs, TInt)
              tcExpr(rhs, TInt)
            }
            case TBoolean => {
              tcExpr(lhs, TBoolean)
              tcExpr(rhs, TBoolean)
            }
            case TString => {
              tcExpr(lhs, TString)
              tcExpr(rhs, TString)
            }
            case TIntArray => {
              tcExpr(lhs, TIntArray)
              tcExpr(rhs, TIntArray)
            }
            case TUntyped => {
              ctx.reporter.error("untype element", lhs)
            }
            case TError => {
              ctx.reporter.error("error element", rhs)
            }
          }
        case ArrayRead(arr, ind) => {
          tcExpr(arr, TIntArray)
          tcExpr(ind, TInt)
        }
        case ArrayLength(arr) => {
          tcExpr(arr, TIntArray)
        }
        case NewIntArray(size) => {
          tcExpr(size, TInt)
        }
        case This() =>
        case MethodCall(obj, meth, args) => {
          tcExpr(obj, obj.getType)
          obj.getType match {
            case TClass(s, _) => // TODO
              meth.setSymbol(s)
            case _ => error("Call method on a wrong type", obj)

          }
        }
        case New(tpe, _)         => // TODO
        case StringLit(value) =>
        case IntLit(value)    =>
        case True()           =>
        case False()          =>
        case Variable(id)     =>
        case _                => error("Undefined expr tree", expr)
      }

      if (!expectedTps.toList.exists(expr.getType.isSubTypeOf)) {

        error("Type error: Expected: " + expectedTps.mkString(" or ") + s", found: ${expr.getType}", expr)
      }

    }

    /** Invokes tcExpr as needed in the expressions of stat */
    def tcStat(stat: StatTree): Unit = {
      stat match {
        case Block(stats) => stats.foreach { s => tcStat(s) }
        case If(expr, thn, els) => {
          tcExpr(expr, TBoolean)
          tcStat(thn)
          els match {
            case Some(s) => tcStat(s)
            case None    =>
          }
        }
        case While(expr, stats) => {
          tcExpr(expr, TBoolean)
          tcStat(stats)
        }
        case Println(expr) => {
          tcExpr(expr, TBoolean, TString, TInt)
        }
        case Assign(id, expr) => {
          tcExpr(expr, id.getType)
        }
        case ArrayAssign(id, index, expr) => {
          if (!(id.getType eq TIntArray)) {
            error("Type error: Expected: TIntArray", id)
          }
          tcExpr(index, TInt)
          tcExpr(expr, TInt)
        }
        case DoExpr(e) => {
          tcExpr(e, e.getType)
        }
      }
    }

    prog.main.stats.foreach(tcStat)
    prog.classes.foreach(tcClass)

    prog
  }
}
