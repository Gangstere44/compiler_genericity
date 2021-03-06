package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import grammarcomp.grammar._
import GrammarUtils.InLL1
import CFGrammar._
import grammarcomp.parsing._
import GrammarDSL._

object Parser extends Pipeline[Iterator[Token], Program] {

  val toolGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'Type ::= INT() ~ LBRACKET() ~ RBRACKET() | BOOLEAN() | INT() | STRING() | 'Identifier,
    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    'Expression ::= 'Expression ~ 'Op ~ 'Expression
      | 'Expression ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | 'Expression ~ DOT() ~ LENGTH()
      | 'Expression ~ DOT() ~ 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN()
      | INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE() | 'Identifier | THIS()
      | NEW() ~ INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | NEW() ~ 'Identifier ~ LPAREN() ~ RPAREN()
      | BANG() ~ 'Expression
      | LPAREN() ~ 'Expression ~ RPAREN(),
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT
  ))

  val ll1Grammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'ClassGenericity ~ 'OptExtends ~ 'ClassBody, // change
    'ClassGenericity ::= LBRACKET() ~ 'Identifier ~ RBRACKET() | epsilon(), // change
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier ~ 'TypeGenericity,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type, 
    'Type ::= INT() ~ 'TypeInt | BOOLEAN() | 'TypeObject, 
    'TypeObject ::= STRING() | 'Identifier ~ 'TypeGenericity, 
    'TypeGenericity ::= LBRACKET() ~ 'TypeObject ~ RBRACKET() | epsilon(),
    'TypeInt ::= epsilon() | LBRACKET() ~ RBRACKET() ,
    
    'Expression ::= 'TermOR ~ 'TermListOR,
    'TermListOR ::= 'OR ~ 'TermOR ~ 'TermListOR | epsilon(),
    'TermOR ::= 'TermAND ~ 'TermListAND,
    'TermListAND ::= 'AND ~ 'TermAND ~ 'TermListAND | epsilon(),
    'TermAND ::= 'TermEQ ~ 'TermListEQ,
    'TermListEQ ::= 'EQUALS ~ 'TermEQ ~ 'TermListEQ 
      | 'LESSTHAN ~ 'TermEQ ~ 'TermListEQ | epsilon(),
    'TermEQ ::= 'TermPLUSMINUS ~ 'TermListPLUSMINUS,
    'TermListPLUSMINUS ::= 'PLUS ~ 'TermPLUSMINUS ~ 'TermListPLUSMINUS 
      | 'MINUS ~ 'TermPLUSMINUS ~ 'TermListPLUSMINUS | epsilon(),
    'TermPLUSMINUS ::= 'TermTIMESDIV ~ 'TermListTIMESDIV,
    'TermListTIMESDIV ::= 'TIMES ~ 'TermTIMESDIV ~ 'TermListTIMESDIV 
      | 'DIV ~ 'TermTIMESDIV ~ 'TermListTIMESDIV | epsilon(),
    'TermTIMESDIV ::= 'TermBANG,
    'TermBANG ::= BANG() ~ 'TermBANG | 'TermBANGBIS,
    'TermBANGBIS ::= 'TermArray ~ 'TermListARRAY,
    'TermListARRAY ::= LBRACKET() ~ 'Expression ~ RBRACKET() | epsilon(),
    'TermArray ::= 'TermDOT ~ 'TermListDOT,
    'TermListDOT ::= DOT() ~ 'TermDOTBIS | epsilon(),
    'TermDOTBIS ::= LENGTH() | 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN() ~ 'TermListDOT,
    'TermDOT ::= NEW() ~ 'TermNewBIS | 'TermNew, 
    'TermNewBIS ::= 'Identifier ~ 'TypeGenericity ~ LPAREN() ~ RPAREN() | INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET(), // change
    'TermNew ::= 'Identifier 
      | TRUE() 
      | FALSE()
      | INTLITSENT
      | STRINGLITSENT
      | THIS()
      | LPAREN() ~ 'Expression ~ RPAREN(),
    'AND ::= AND(),
    'OR ::= OR(),
    'EQUALS ::= EQUALS(),
    'LESSTHAN ::= LESSTHAN(),
    'PLUS ::= PLUS(),
    'MINUS ::= MINUS(),
    'TIMES ::= TIMES(),
    'DIV ::= DIV(),
             
    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
    'Identifier ::= IDSENT
    
  ))

  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()
    val list = tokens.toList
    GrammarUtils.isLL1WithFeedback(ll1Grammar) match {
      case InLL1() =>
        info("Grammar is in LL1")
      case other =>
        warning(other)
    }
    val feedback = ParseTreeUtils.parseWithTrees(ll1Grammar, list)
    feedback match {
      case s: Success[Token] =>
        (new ASTConstructorLL1).constructProgram(s.parseTrees.head)
      case fdb =>
        fatal("Parsing failed: "+fdb)
    }
  }

}
