package toolc
package ast

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import grammarcomp.parsing._

class ASTConstructorLL1 extends ASTConstructor {

  override def constructClass(ptree: NodeOrLeaf[Token]): ClassDecl = {
    ptree match {
      case Node(
        'ClassDeclaration ::= _,
        List(Leaf(cls), id, clgen, optextends, Node('ClassBody ::= _, List(_, vardecls, methoddecls, _)))
        ) =>
        ClassDecl(
          constructId(id),
          constructOption(clgen, constructId), // change
          constructOption(optextends, constructId),
          constructList(vardecls, constructVarDecl),
          constructList(methoddecls, constructMethodDecl)).setPos(cls)
    }
  }
 
  override def constructOption[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A): Option[A] = {
    ptree match {
      case Node(_, List()) => None
      case Node(_, List(_, t)) =>
        Some(constructor(t))
      case Node(_, List(_, t, _)) => // change
        Some(constructor(t))
    }
  }
  
  /* original
  override def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= _, List(Leaf(i @ INT()), ti)) =>
        constructTypeInt(ti).setPos(i)
      case Node('Type ::= _, List(Leaf(b @ BOOLEAN()))) =>
        BooleanType().setPos(b)
      case Node('Type ::= _, List(Leaf(s @ STRING()))) =>
        StringType().setPos(s)
      case Node('Type ::= List(IDSENT), List(id)) =>
        val pid = constructId(id)
        ClassType(pid).setPos(pid)
      case Node('Type ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        ClassType(pid).setPos(pid)
    }
  }
  */
  
    override def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= _, List(Leaf(i @ INT()), ti)) =>
        constructTypeInt(ti).setPos(i)
      case Node('Type ::= _, List(Leaf(b @ BOOLEAN()))) =>
        BooleanType().setPos(b)
      case Node('Type ::= _, List(t)) =>
        constructTypeObject(t) // change
    }
  }

  def constructTypeInt(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('TypeInt ::= _, Nil) =>
        IntType()
      case Node('TypeInt ::= List(LBRACKET(), RBRACKET()), List(_, _)) =>
        IntArrayType()
    }
  }
  
  // add
  def constructTypeObject(ptree: NodeOrLeaf[Token]) : ObjectTypeTree = {
    ptree match {
      case Node('TypeObject ::= _, List(Leaf(s @ STRING()))) =>
        StringType().setPos(s)
      case Node('TypeObject ::= List('Identifier, 'TypeGenericity), List(id, vargen)) =>
        val pid = constructId(id)
        val gen = constructOption(vargen, constructTypeObject)
        
        ClassType(pid, gen).setPos(pid)
    }
  }

  override def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('Expression ::= List('TermOR, _), List(to, tlo)) => {
        val e1 = constructExpr(to)
        val e2 = constructListOption(e1, tlo)
        e2 match {
          case Some(e) => e.setPos(e1)
          case None    => e1.setPos(e1)
        }
      }
      case Node('TermOR ::= List('TermAND, _), List(ta, tla)) => {
        val e1 = constructExpr(ta)
        val e2 = constructListOption(e1, tla)
        e2 match {
          case Some(e) => e.setPos(e1)
          case None    => e1.setPos(e1)
        }
      }
      case Node('TermAND ::= List('TermEQ, _), List(ta, tla)) => {
        val e1 = constructExpr(ta)
        val e2 = constructListOption(e1, tla)
        e2 match {
          case Some(e) => e.setPos(e1)
          case None    => e1.setPos(e1)
        }
      }
      case Node('TermEQ ::= List('TermPLUSMINUS, _), List(tpm, tlpm)) => {
        val e1 = constructExpr(tpm)
        val e2 = constructListOption(e1, tlpm)
        e2 match {
          case Some(e) => e.setPos(e1)
          case None    => e1.setPos(e1)
        }
      }
      case Node('TermPLUSMINUS ::= List('TermTIMESDIV, _), List(ttd, tltd)) => {
        val e1 = constructExpr(ttd)
        val e2 = constructListOption(e1, tltd)
        e2 match {
          case Some(e) => e.setPos(e1)
          case None    => e1.setPos(e1)
        }
      }
      case Node('TermTIMESDIV ::= List('TermBANG), List(tb)) => {
        val e1 = constructExpr(tb)
        e1.setPos(e1)
      }
      case Node('TermBANG ::= List(_, 'TermBANG), List(Leaf(op), tbb)) => {
        Not(constructExpr(tbb)).setPos(op)
      }
      case Node('TermBANG ::= List('TermBANGBIS), List(tbb)) => {
        val e1 = constructExpr(tbb)
        e1.setPos(e1)
      }
      case Node('TermBANGBIS ::= List('TermArray, _), List(tar, tlar)) => {
        val e1 = constructExpr(tar)
        val e2 = constructListOption(e1, tlar)
        e2 match {
          case Some(e) => e.setPos(e1)
          case None    => e1.setPos(e1)
        }
      }
      case Node('TermArray ::= List('TermDOT, _), List(tdo, tldo)) => {
        val e1 = constructExpr(tdo)
        val e2 = constructListOption(e1, tldo)
        e2 match {
          case Some(e) => e.setPos(e1)
          case None    => e1.setPos(e1)
        }
      }
      case Node('TermDOT ::= List(_, 'TermNewBIS), List(Leaf(nt), tnb)) => {
        constructExpr(tnb).setPos(nt)
      }
      case Node('TermDOT ::= List('TermNew), List(tn)) => {
        val e1 = constructExpr(tn)
        e1.setPos(e1)
      }
      case Node('TermNewBIS ::= List('Identifier, 'TypeGenericity ,LPAREN(), RPAREN()), List(tn, tg,_, _)) => {
        val id = constructId(tn)
        val gen = constructOption(tg, constructTypeObject) // change
        New(id, gen).setPos(id)
      }
      case Node('TermNewBIS ::= List(INT(), LBRACKET(), 'Expression, RBRACKET()), List(Leaf(i), _, ex, _)) => {
        NewIntArray(constructExpr(ex)).setPos(i)
      }
      case Node('TermNew ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        Variable(pid).setPos(pid)
      case Node('TermNew ::= List(INTLITSENT), List(Leaf(it @ INTLIT(i)))) =>
        IntLit(i).setPos(it)
      case Node('TermNew ::= List(STRINGLITSENT), List(Leaf(st @ STRINGLIT(s)))) =>
        StringLit(s).setPos(st)
      case Node('TermNew ::= _, List(Leaf(tt @ TRUE()))) =>
        True().setPos(tt)
      case Node('TermNew ::= _, List(Leaf(tf @ FALSE()))) =>
        False().setPos(tf)
      case Node('TermNew ::= _, List(Leaf(tt @ THIS()))) =>
        This().setPos(tt)
      case Node('TermNew ::= List(LPAREN(), 'Expression, RPAREN()), List(Leaf(lp), e, _)) =>
        constructExpr(e).setPos(lp)
    }
  }

  def constructListOption(ptreeLeft: ExprTree, ptreeRight: NodeOrLeaf[Token]): Option[ExprTree] = {
    // println("---")
    // println(ptreeLeft + " " + ptreeRight)
    ptreeRight match {
      case Node('TermListOR ::= List(_, 'TermOR, 'TermListOR), List(op, to, tlo)) => {
        val e2 = constructExpr(to)
        val e3 = constructOp(op)(ptreeLeft, e2)
        val e4 = constructListOption(e3, tlo)
        e4 match {
          case Some(e) => Some(e.setPos(ptreeLeft))
          case None    => Some(e3.setPos(ptreeLeft))
        }
      }
      case Node('TermListOR ::= _, _) =>
        None
      case Node('TermListAND ::= List(_, 'TermAND, 'TermListAND), List(op, ta, tla)) => {
        val e2 = constructExpr(ta)
        val e3 = constructOp(op)(ptreeLeft, e2)
        val e4 = constructListOption(e3, tla)
        e4 match {
          case Some(e) => Some(e.setPos(ptreeLeft))
          case None    => Some(e3.setPos(ptreeLeft))
        }
      }
      case Node('TermListAND ::= _, _) =>
        None
      case Node('TermListEQ ::= List(_, 'TermEQ, 'TermListEQ), List(op, te, tle)) => {
        val e2 = constructExpr(te)
        val e3 = constructOp(op)(ptreeLeft, e2)
        val e4 = constructListOption(e3, tle)
        e4 match {
          case Some(e) => Some(e.setPos(ptreeLeft))
          case None    => Some(e3.setPos(ptreeLeft))
        }
      }
      case Node('TermListEQ ::= _, _) =>
        None
      case Node('TermListPLUSMINUS ::= List(_, 'TermPLUSMINUS, 'TermListPLUSMINUS), List(op, tpm, tlpm)) => {
        val e2 = constructExpr(tpm)
        val e3 = constructOp(op)(ptreeLeft, e2)
        val e4 = constructListOption(e3, tlpm)
        e4 match {
          case Some(e) => Some(e.setPos(ptreeLeft))
          case None    => Some(e3.setPos(ptreeLeft))
        }
      }
      case Node('TermListPLUSMINUS ::= _, _) =>
        None
      case Node('TermListTIMESDIV ::= List(_, 'TermTIMESDIV, 'TermListTIMESDIV), List(op, ttd, tltd)) => {
        val e2 = constructExpr(ttd)
        val e3 = constructOp(op)(ptreeLeft, e2)
        val e4 = constructListOption(e3, tltd)
        e4 match {
          case Some(e) => Some(e.setPos(ptreeLeft))
          case None    => Some(e3.setPos(ptreeLeft))
        }
      }
      case Node('TermListTIMESDIV ::= _, _) =>
        None
      case Node('TermListARRAY ::= List(LBRACKET(), 'Expression, RBRACKET()), List(Leaf(l), ex, _)) => {
        val e1 = constructExpr(ex)
        Some(ArrayRead(ptreeLeft, e1).setPos(ptreeLeft))
      }
      case Node('TermListARRAY ::= _, _) =>
        None
      case Node('TermListDOT ::= List(_, 'TermDOTBIS), List(_, td)) => {
        constructListOption(ptreeLeft, td)
      }
      case Node('TermListDOT ::= _, _) =>
        None
      case Node('TermDOTBIS ::= List(LENGTH()), List(Leaf(op))) =>
        Some(ArrayLength(ptreeLeft).setPos(ptreeLeft))
      case Node('TermDOTBIS ::= List('Identifier, LPAREN(), 'Args, RPAREN(), 'TermListDOT), List(id, _, ar, _, tld)) => {
        val e1 = MethodCall(ptreeLeft, constructId(id), constructList(ar, constructExpr, hasComma = true))
        val e2 = constructListOption(e1, tld)
        e2 match {
          case Some(e) => Some(e.setPos(ptreeLeft))
          case None    => Some(e1.setPos(ptreeLeft))
        }
      }

    }
  }

}
