package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import scala.collection.immutable.HashMap

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def collectSymbols(prog: Program): GlobalScope = {

      val global = new GlobalScope

      val mcSym = new MainSymbol(prog.main.id.value)
      global.mainClass = mcSym
      prog.main.setSymbol(mcSym)
      prog.main.id.setSymbol(mcSym)

      if (global.mainClass.name.equals("Object")) {
        error("the main object is called object", prog.main.id)
      }
      for (c <- prog.classes) {
        val cS = new ClassSymbol(c.id.value) //c.gen.map { x => x.value })
        if (c.gen.isDefined) {
          val gS = new GenericSymbol(c.gen.get.value, cS) // TODO pour multiple
          c.gen.get.setSymbol(gS)
          cS.gen.+=(c.gen.get.value -> gS)
        }
        if (cS.name.equals(global.mainClass.name)) {
          error("Class called like the main forbidden", c.id)
        } else if (cS.name.equals("Object")) {
          error("Class called Object is forbidden", c.id)
        } else if (!global.classes.contains(cS.name)) {
          c.setSymbol(cS)
          c.id.setSymbol(cS)
          global.classes += (cS.name -> cS)
        } else {
          error("multiple name occurence for classes definition", c.id)
        }
      }

      // Set parent Symbols
      for {
        cls <- prog.classes
        clSym = global.classes(cls.id.value)
        par <- cls.parent
      } yield {
        global.lookupClass(par.value) match {
          case None =>
            error(s"Class ${clSym.name} extends class ${par.value} which is not defined.", par)
          case Some(parSym) =>
            clSym.parent = Some(parSym)
            par.setSymbol(parSym)
        }
      }

      // Check there are no cycles in the inheritance graph
      prog.classes foreach { cls =>
        val clsSym = cls.getSymbol

        def mkChain(curr: ClassSymbol): List[ClassSymbol] = {
          curr.parent match {
            case None           => List(curr)
            case Some(`clsSym`) => List(curr, clsSym)
            case Some(p)        => curr :: mkChain(p)
          }
        }

        val chain = mkChain(clsSym)

        if (chain.size > 1 && chain.head == chain.last) {
          fatal("Cyclic inheritance: " + chain.map(_.name).mkString(" -> "))
        }

      }

      // We now know that every class is unique and the inheritance graph is
      // correct. We proceed to check the contents of these classes.
      // create a map to ease the next instructions
      def createMapClassDecl(rem: List[ClassDecl], m: Map[ClassSymbol, ClassDecl]): Map[ClassSymbol, ClassDecl] = {

        if (rem.isEmpty) {
          m
        } else {

          val symbol = global.lookupClass(rem.head.id.value)

          symbol match {
            case Some(s) => createMapClassDecl(rem.tail, m.+(s -> rem.head))
            case None =>
              sys.error("Symbol not created for a ClassDecl")
              createMapClassDecl(rem.tail, m)
          }
        }
      }

      val mapSymToDecl = createMapClassDecl(prog.classes, new HashMap[ClassSymbol, ClassDecl]());
      var done: Set[String] = Set()
      prog.classes.foreach(c => collectInClass(c, mapSymToDecl))

      def collectInClass(c: ClassDecl, mapSymToDecl: Map[ClassSymbol, ClassDecl]): Unit = {

        def constructTypeSymbol(currentCSym: ClassSymbol, cur: TypeTree): Types.Type = {
          cur match {
            case c: ClassType => {
              global.lookupClass(c.id.value) match {
                case Some(cSym: ClassSymbol) => {
                  Types.TClass(cSym, c.gen.map { x => constructTypeSymbol(currentCSym, x) })
                }
                case None => {
                  currentCSym.lookupGen(c.id.value) match {
                    case Some(gS) => {
                      c.id.setSymbol(gS)
                      gS.getType
                    }
                    case None => Types.TError

                  }
                }
              }
            }
            case c => c.getType
          }
        }

        def collectInternal(cS: ClassSymbol): Unit = {
          done = done.+(cS.name)

          val parentDecl: ClassDecl = mapSymToDecl.get(cS).get

          // add the class variable of the parent to the class scope
          for (varClass <- parentDecl.vars) {
            val varS = new VariableSymbol(varClass.id.value);
            varS.setType(constructTypeSymbol(cS, varClass.tpe))
            /*
            varClass.tpe match {
              case ClassType(t, g) => global.lookupClass(t.value) match { 
                 case Some(sym) => {
                  g match {
                    case Some(genericity) => 
                    case None => varS.setType(sym.getType)
                  }
                  varS.setType(Types.TClass(NodeSymbole, Some(TClass(NodeSymbol, Some(TGeneric(Node[T])))))
                  
                }
                
                case None => {
                  if (cS.gen.getOrElse("") == t.value && !g.isDefined) {
                    varS.setType(Types.TGeneric(cS))
                  } else {
                    varS.setType(Types.TError)
                  }
                }
              }
              case _ => varS.setType(varClass.tpe.getType) // TYPE
            }
            */
            cS.lookupVar(varS.name) match {
              case Some(v) => error("Multiple definition of a variable in a class", varClass.id)
              case None => {
                varClass.setSymbol(varS)
                varClass.id.setSymbol(varS)
                cS.members += (varS.name -> varS)
              }
            }
          }

          // add the method of the parent to the class scope
          for (meth <- parentDecl.methods) {
            val method: MethodSymbol = new MethodSymbol(meth.id.value, cS);
            meth.setSymbol(method)
            meth.id.setSymbol(method)
            method.setType(constructTypeSymbol(cS, meth.retType))

            /*
            meth.retType match {
              case ClassType(t, _) => global.lookupClass(t.value) match {
                case Some(c) => method.setType(c.getType)
                case None    => method.setType(Types.TError)
              }
              case _ => method.setType(meth.retType.getType) // TYPE
            }
						*/

            // add the arguments of a method
            for (argMeth <- meth.args) {
              if (method.params.contains(argMeth.id.value)) {
                error("Two times the same argument name inside the method", argMeth.id)
              } else {
                val argS: VariableSymbol = new VariableSymbol(argMeth.id.value)
                argMeth.setSymbol(argS)
                argMeth.id.setSymbol(argS)
                argS.setType(constructTypeSymbol(cS, argMeth.tpe))

                /*
                argMeth.tpe match {
                  case ClassType(t, _) => global.lookupClass(t.value) match { 
                    case Some(c) => argS.setType(c.getType)
                    case None    => argS.setType(Types.TError)
                  }
                  case _ => argS.setType(argMeth.tpe.getType) // TYPE
                }
                */

                method.params += (argS.name -> argS)
                method.argList = method.argList.+:(argS)
              }
            }

            // add the variable of a method
            for (varMeth <- meth.vars) {
              if (method.params.contains(varMeth.id.value)) {
                error("Internal variable has the same name as an argument of the method", varMeth.id)
              } else if (method.members.contains(varMeth.id.value)) {
                error("Two variables with the same name inside a method", varMeth.id)
              } else {
                val varS = new VariableSymbol(varMeth.id.value)
                varMeth.setSymbol(varS)
                varMeth.id.setSymbol(varS)
                varS.setType(constructTypeSymbol(cS, varMeth.tpe))

                /*
                varMeth.tpe match {
                  case ClassType(t, _) => global.lookupClass(t.value) match { 
                    case Some(c) => varS.setType(c.getType)
                    case None    => varS.setType(Types.TError)
                  }
                  case _ => varS.setType(varMeth.tpe.getType) // TYPE
                }
                */

                method.members += (varS.name -> varS)
              }
            }

            // check method correctness if overriding
            cS.lookupMethod(method.name) match {

              case Some(me) => {
                // if the founded method was defined inside the same class
                // or the number of argument doesn't match

                if (me.classSymbol.equals(cS) || me.argList.size != method.argList.size) {
                  error("Try to override in a wrong way, either already defined in the class or the number of element doesn't match", meth.id)
                } else {
                  val argTypeCheck = (for ((a1, a2) <- me.argList zip method.argList) yield a2.getType.toString() eq a1.getType.toString())
                  if (!(me.getType.toString() eq method.getType.toString())) {
                    error("Try to override in a wrong way by the return type, shoudl be : " + me.getType + " is : " + method.getType, meth.id)
                  } else if ((argTypeCheck.size > 0 && !argTypeCheck.reduceLeft(_ && _))) { // TYPE
                    error("Try to override in a wrong way by the Type for one of the argument", meth.id)
                  } else {
                    method.overridden = Option(me)
                    cS.methods += (method.name -> method)
                  }
                }
              }
              case None => cS.methods += (method.name -> method)
            }

          }
        }

        // collect parent in a recursive way
        def collectParent(parentSymbol: ClassSymbol): Unit = {
          parentSymbol.parent match {
            case Some(par) => {
              collectParent(par)
              parentSymbol.members = parentSymbol.members.++(par.members)
              parentSymbol.methods = parentSymbol.methods.++(par.methods)
              if (!done.contains(parentSymbol.name)) {
                collectInternal(parentSymbol)
              }
            }
            case None => {
              if (!done.contains(parentSymbol.name)) {
                collectInternal(parentSymbol)
              }
            }

          }

        }

        // get the scope 
        var cc: ClassSymbol = global.lookupClass(c.id.value).get

        collectParent(cc)
      }
      global
    }

    def setPSymbols(prog: Program, gs: GlobalScope): Unit = {

      prog.main.stats.foreach { s => setSSymbols(s)(gs, None) }
      prog.classes.foreach { c => setCSymbols(c, gs) }
    }

    def setCSymbols(klass: ClassDecl, gs: GlobalScope): Unit = {
      val classSym = gs.lookupClass(klass.id.value).get
      for (varDecl <- klass.vars) {
        setTypeSymbol(varDecl.tpe, Some(classSym), gs)
      }
      klass.methods.foreach(setMSymbols(_, gs, classSym))
    }

    def setMSymbols(meth: MethodDecl, gs: GlobalScope, cs: ClassSymbol): Unit = {

      val methSym = cs.lookupMethod(meth.id.value).get

      for (argDecl <- meth.args) {
        setTypeSymbol(argDecl.tpe, Some(cs), gs)
      }

      for (varDecl <- meth.vars) {
        setTypeSymbol(varDecl.tpe, Some(cs), gs)
      }

      setTypeSymbol(meth.retType, Some(cs), gs)

      meth.stats.foreach { x => setSSymbols(x)(gs, Option(methSym)) }

      setESymbols(meth.retExpr)(gs, Option(methSym))
    }

    def setSSymbols(stat: StatTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {

      stat match {
        case Block(l) => l.foreach { s => setSSymbols(s) }
        case If(e, s, o) => {
          setESymbols(e)
          setSSymbols(s)
          o match {
            case Some(os) => setSSymbols(os)
            case None     =>
          }
        }
        case While(e, s) => {
          setESymbols(e)
          setSSymbols(s)
        }
        case Println(e) => setESymbols(e)
        case Assign(id, e) => {
          setISymbol(id)
          setESymbols(e)
        }
        case ArrayAssign(id, e, ex) => {
          setISymbol(id)
          setESymbols(e)
          setESymbols(ex)
        }
        case DoExpr(e) => setESymbols(e)
      }

    }

    def setISymbol(id: Identifier)(implicit ms: Option[MethodSymbol]) = {
      // in this context, it will always be an expression (variable)
      ms.flatMap(_.lookupVar(id.value)) match {
        case None =>
          error("Undeclared identifier: " + id.value + ".", id)
        case Some(sym) =>
          id.setSymbol(sym)
      }
    }

    def setESymbols(expr: ExprTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {

      expr match {
        case Variable(id) => {
          setISymbol(id)
        }
        case New(tpe, optGen) => {
          gs.lookupClass(tpe.value) match {
            case Some(c) => {
              tpe.setSymbol(c)
              if(optGen.isDefined) {
                setTypeSymbol(optGen.get, ms.map { x => x.classSymbol }, gs)
              }
            }
            case None => error("Undeclared identifier: " + tpe.value + ".", tpe)
          }
        }
        case MethodCall(o, m, a) => {
          setESymbols(o)

          a.foreach { e => setESymbols(e) }
        }
        case x: This => {

          x.setSymbol(ms.get.classSymbol)
        }
        case And(l, r) => {
          setESymbols(l)
          setESymbols(r)
        }
        case Or(l, r) => {
          setESymbols(l)
          setESymbols(r)
        }
        case Plus(l, r) => {
          setESymbols(l)
          setESymbols(r)
        }
        case Minus(l, r) => {
          setESymbols(l)
          setESymbols(r)
        }
        case Times(l, r) => {
          setESymbols(l)
          setESymbols(r)
        }
        case Div(l, r) => {
          setESymbols(l)
          setESymbols(r)
        }
        case LessThan(l, r) => {
          setESymbols(l)
          setESymbols(r)
        }
        case Equals(l, r) => {
          setESymbols(l)
          setESymbols(r)
        }
        case ArrayRead(l, r) => {
          setESymbols(l)
          setESymbols(r)
        }
        case ArrayLength(o) => setESymbols(o)
        case NewIntArray(s) => setESymbols(s)
        case Not(e)         => setESymbols(e)
        case _              =>
      }

    }

    def setTypeSymbol(tpe: TypeTree, currentClassSym: Option[ClassSymbol], gs: GlobalScope): Unit = {
      tpe match {
        case ClassType(id, g) => {
          gs.lookupClass(id.value) match {
            case Some(c) => {
              id.setSymbol(c)
              if (g.isDefined) {
                setTypeSymbol(g.get, currentClassSym, gs)
              }
            }
            case None => {
              currentClassSym match {
                case Some(curCSym) => {
                   curCSym.lookupGen(id.value) match {
                    case Some(gSym) => id.setSymbol(gSym)
                    case None       => error("Undeclared identifier: " + id.value + ".", id)
                  }
                }
                case None => error("Undeclared identifier: " + id.value + ".", id)
              }

            }
          }
        }

        case _ =>
      }
    }

    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    prog
  }
}
