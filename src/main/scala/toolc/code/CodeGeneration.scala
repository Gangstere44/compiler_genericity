package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{ New => _, _ }
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  // A mapping from a parameter/local variable name to the index of this parameter/variable
  // in the fields of a method
  type LocalsPosMapping = Map[String, Int]

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /**** Helper methods ****/

    def generateClassFile(ct: ClassDecl, shortFileName: String, outDir: String): Unit = {
      val cs = ct.getSymbol
      val cf = new ClassFile(cs.name, cs.parent.map(_.name))
      cf.setSourceFile(shortFileName)
      cf.addDefaultConstructor

      ct.vars.foreach { v => cf.addField(typeToDescr(v.tpe.getType), v.id.value) }

      ct.methods.foreach { m =>
        cGenMethod(cf.addMethod(
          typeToDescr(m.retType.getType),
          m.id.value,
          m.args.map { a => typeToDescr(a.tpe.getType) }).codeHandler,
          m)
      }

      writeClassFile(cf, outDir, cs.name)
    }

    def generateMainClassFile(main: MainObject, sourceFileName: String, outDir: String): Unit = {
      // Main class has a special handling
      val cs = main.getSymbol
      val mainClassFile = new ClassFile(cs.name, None)
      mainClassFile.setSourceFile(sourceFileName)
      mainClassFile.addDefaultConstructor

      cGenMain(
        mainClassFile.addMainMethod.codeHandler,
        prog.main.stats,
        cs.name)

      writeClassFile(mainClassFile, outDir, cs.name)
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def writeClassFile(cf: ClassFile, dir: String, className: String) = {
      try {
        cf.writeToFile(dir + className + ".class")
      } catch {
        case e: Exception => fatal(e.getMessage)
      }
    }

    def cGenMethod(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // Maps each argument to one local variable index position
      val argMappings = mt.args.zipWithIndex.map {
        case (arg, index) =>
          arg.id.getSymbol.name -> (index + 1)
      }.toMap

      // Maps each variable to one local variable index position
      val variableMappings = mt.vars.map(v => v.getSymbol.name -> ch.getFreshVar).toMap

      val mapping = argMappings ++ variableMappings

      mt.stats.foreach { s => cGenStat(s)(ch, mapping, mt.getSymbol.classSymbol.name) }

      ch << LineNumber(mt.retExpr.line)
      cGenExpr(mt.retExpr)(ch, mapping, mt.getSymbol.classSymbol.name)

      mt.retType.getType match {
        case TInt     => ch << IRETURN
        case TBoolean => ch << IRETURN
        case _        => ch << ARETURN

      }

      ch.freeze

    }

    // Generates code for the main method
    def cGenMain(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      stmts.foreach { s => cGenStat(s)(ch, Map(), cname) }

      ch << RETURN

      ch.freeze
    }

    // Generates code for a statement
    def cGenStat(statement: StatTree)(implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      ch << LineNumber(statement.line)
      statement match {

        case Block(stats) => {
          stats foreach { x =>
            cGenStat(x)
          }
        }
        case If(expr, thn, els) => {
          ch << ICONST_0
          cGenExpr(expr)

          val elseJump = ch.getFreshLabel("elseJump")
          ch << IfEq(elseJump)

          val endIfCond = ch.getFreshLabel("endIfCond")
          cGenStat(thn)
          ch << Goto(endIfCond)

          ch << Label(elseJump)

          els match {
            case Some(s) => {
              cGenStat(s)
            }
            case None    =>
          }

          ch << Label(endIfCond)
          ch << POP
        }
        case While(expr, stat) => {

          val startLoop = ch.getFreshLabel("startLoop")
          val endLoop = ch.getFreshLabel("endLoop")

          ch << Label(startLoop)

          ch << ICONST_0
          cGenExpr(expr)
          ch << IfEq(endLoop)
          ch << POP

          cGenStat(stat)
          ch << Goto(startLoop)

          ch << Label(endLoop)
          ch << POP

        }
        case Println(expr) => {

          ch << LineNumber(expr.line)
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          cGenExpr(expr)
          ch << InvokeVirtual("java/io/PrintStream", "println", "(" + typeToDescr(expr.getType) + ")V")

        }
        case Assign(id, expr) => {

          mapping.get(id.value) match {
            case Some(i) => {
              cGenExpr(expr)
              expr.getType match {
                case TInt     => ch << IStore(i)
                case TBoolean => ch << IStore(i)
                case _        => ch << AStore(i)
              }
            }
            case None => {
              ch << ALoad(0)
              cGenExpr(expr)
              ch << PutField(cname, id.value, typeToDescr(expr.getType))
            }
          }

        }
        case ArrayAssign(id, index, expr) => {

          mapping.get(id.value) match {
            case Some(i) =>
              ch << ALoad.apply(i)
            case None =>
              ch << ALoad(0)
              ch << GetField(cname, id.value, "[I")
          }

          cGenExpr(index)

          cGenExpr(expr)

          ch << IASTORE

        }
        case DoExpr(e) => {
          cGenExpr(e)
          ch << POP
        }

        case _ => error("unknown statement in code gen", statement)
      }
    }

    // Generates code for an expression
    def cGenExpr(expr: ExprTree)(implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      expr match {
        case And(lhs, rhs) => {
          ch << ICONST_0
          cGenExpr(lhs)

          val alreadyFalse = ch.getFreshLabel("alreadyFalse")
          ch << IfEq(alreadyFalse)

          // Only care about the right hand side value
          ch << POP
          cGenExpr(rhs)

          ch << Label(alreadyFalse)
        }
        case Or(lhs, rhs) => {
          
          ch << ICONST_1
          cGenExpr(lhs)

          val alreadyTrue = ch.getFreshLabel("alreadyTrue")
          ch << IfNe(alreadyTrue)

          ch << POP
          cGenExpr(rhs)

          ch << Label(alreadyTrue)

        }
        case Not(expr) => {

          cGenExpr(expr)
          ch << ICONST_1

          val wasOne = ch.getFreshLabel("wasOne")
          val endNot = ch.getFreshLabel("endNot")

          ch << If_ICmpEq(wasOne)

          ch << ICONST_1
          ch << Goto(endNot)

          ch << Label(wasOne)
          ch << ICONST_0

          ch << Label(endNot)
        }
        case Plus(lhs, rhs) => {

          expr.getType match {

            case TInt => {
              cGenExpr(lhs)
              cGenExpr(rhs)
              ch << IADD
            }
            case TString => {
              val sb = "java/lang/StringBuilder"

              ch << DefaultNew(sb)
              cGenExpr(lhs)
              ch << InvokeVirtual(sb, "append", "(" + typeToDescr(lhs.getType) + ")" + "L" + sb + ";")
              cGenExpr(rhs)
              ch << InvokeVirtual(sb, "append", "(" + typeToDescr(rhs.getType) + ")" + "L" + sb + ";")
              ch << InvokeVirtual(sb, "toString", "()Ljava/lang/String;")

            }

            case _ => error("wrong type for Plus in code gen", expr)
          }
        }
        case Minus(lhs, rhs) => {
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << ISUB
        }
        case Times(lhs, rhs) => {
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << IMUL
        }
        case Div(lhs, rhs) => {
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << IDIV
        }
        case LessThan(lhs, rhs) => {
          cGenExpr(lhs)
          cGenExpr(rhs)

          val correctLt = ch.getFreshLabel("correctLt")
          val endLt = ch.getFreshLabel("endLt")

          ch << If_ICmpLt(correctLt)

          ch << ICONST_0
          ch << Goto(endLt)

          ch << Label(correctLt)

          ch << ICONST_1

          ch << Label(endLt)
        }

        case Equals(lhs, rhs) => {
          cGenExpr(lhs)
          cGenExpr(rhs)

          val correctEq = ch.getFreshLabel("correctEq")
          val endEq = ch.getFreshLabel("endEq")

          lhs.getType match {
            case TInt => {
              ch << If_ICmpEq(correctEq)
            }
            case TBoolean => {
              ch << If_ICmpEq(correctEq)
            }
            case _ => {
              ch << If_ACmpEq(correctEq)
            }
          }

          ch << ICONST_0
          ch << Goto(endEq)

          ch << Label(correctEq)

          ch << ICONST_1

          ch << Label(endEq)
        }
        case ArrayRead(arr, idx) => {

          cGenExpr(arr)
          cGenExpr(idx)
          ch << IALOAD

        }
        case ArrayLength(arr) => {

          cGenExpr(arr)
          ch << ARRAYLENGTH

        }
        case NewIntArray(size) => {

          cGenExpr(size)
          ch << NewArray(10)

        }
        case This() => {

          ch << ALoad(0)

        }
        case MethodCall(obj, meth, args) => {

          cGenExpr(obj)
          args.foreach { a => cGenExpr(a) }
          val tmpArg = {
            if (args.isEmpty)
              "()"
            else
              "(" + (args.map { a => typeToDescr(a.getType) }.reduce(_ + _)) + ")"
          }

          obj.getType match {
            case TClass(c) => {
              c.lookupMethod(meth.value) match {
                case Some(m) => {
                  ch << InvokeVirtual(c.name, meth.value, tmpArg + typeToDescr(m.getType))
                }
                case None => error("methode not found for object : " + c.name + " , in code gen", expr)
              }
            }
            case _ => error("try to call a method againt sth not an object", expr)
          }

        }
        case New(tpe) => {

          tpe.getType match {
            case TClass(c) => {
              ch << DefaultNew(c.name)
            }
            case _ => error("try to 'new' a primary type", expr)
          }
        }
        case IntLit(v) => {

          ch << Ldc(v)

        }
        case StringLit(v) => {

          ch << Ldc(v)

        }
        case True() => {

          ch << ICONST_1
        }
        case False() => {

          ch << ICONST_0

        }
        case Variable(id) => {

          mapping.get(id.value) match {
            case Some(i) =>
              id.getType match {
                case TInt     => ch << ILoad.apply(i)
                case TBoolean => ch << ILoad.apply(i)
                case _        => ch << ALoad.apply(i)
              }
            case None =>
              ch << ALoad(0)
              ch << GetField(cname, id.value, typeToDescr(id.getType))
          }

        }
        case _ => error("unknown expr in code gen", expr)
      }

    }

    // Transforms a Tool type to the corresponding JVM type description
    def typeToDescr(t: Type): String = {
      t match {
        case TInt      => "I"
        case TBoolean  => "Z"
        case TString   => "Ljava/lang/String;"
        case TIntArray => "[I"
        case TClass(c) => "L" + c.name + ";"

        case _ =>
          error("unknown type in code gen")
          ""
      }
    }

    /**** Main code ****/

    // Make output directory
    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")
    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    // Name of source file (to track positions)
    val sourceName = ctx.files.head.getName

    // output class code
    prog.classes foreach {
      generateClassFile(_, sourceName, outDir)
    }

    // output main object code
    generateMainClassFile(prog.main, sourceName, outDir)

  }

}

