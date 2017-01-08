package toolc
package analyzer

import Symbols._

object Types {
  trait Typed {
    def getType: Type
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean = tpe == this
    def toStringRec(curType: Option[Type]): String = {
      curType match {
        case Some(TClass(cs, optg)) => if (optg.isDefined) s"${cs.name}[${toStringRec(optg)}]" else s"${cs.name}"
        case Some(r)                => r.toString()
        case None                   => ""
      }
    }
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def toString = "Int"
  }

  case object TBoolean extends Type {
    override def toString = "Bool"
  }

  case object TString extends Type {
    override def toString = "String"
  }

  case object TIntArray extends Type {
    override def toString = "Int[]"
  }

  case class TClass(classSymbol: ClassSymbol, genType: Option[Type]) extends Type {

    override def isSubTypeOf(tpe: Type): Boolean = {

      def checkSub(cl: ClassSymbol): Boolean = {
        if (cl.getType.toString() eq tpe.toString())
          true
        else {
          cl.parent match {
            case Some(p) =>
              checkSub(p.classSymbol)
            case None => false
          }
        }
      }

      def checkTypeGen(tp1: Type, tp2: Type): Boolean = {
        (tp1, tp2) match {
          case (TClass(cS1, gen1), TClass(cS2, gen2)) => {
            (gen1, gen2) match {
              case (None, None) => true
              case (Some(a), Some(b)) => {
                // Node[Node[String]] != Node[Fact[String]]
                (a.toString() eq b.toString()) && checkTypeGen(a, b)
              }
              case _ => false
            }
          }
          case (TGeneric(n1, _), TGeneric(n2, _)) => {
            n1 eq n2
          }
          case (TString, TString) => true
          case _                  => false
        }
      }

      (tpe eq TObject) ||
        (checkSub(classSymbol) && checkTypeGen(tpe, this))
    }

    override def toString = classSymbol.name

  }

  // in Fact[T] , TGeneric(Fact[T]) -> interesting for T only, T is TGeneric
  case class TGeneric(val name: String, val linkedClass: ClassSymbol) extends Type {
    override def toString = name
  }

  // The top of the class hierarchy. Does not correspond to anything in a Tool program,
  // we just use if for convenience during type checking.
  val TObject = TClass(new ClassSymbol("Object"), None) // change
}
