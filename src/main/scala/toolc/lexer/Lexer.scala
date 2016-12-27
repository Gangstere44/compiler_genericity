package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  /**
   * Maps a string s to the corresponding keyword,
   * or None if it corresponds to no keyword
   */

  private def keywords(s: String): Option[Token] = s match {
    case "program" => Some(PROGRAM())
    case "class"   => Some(CLASS())
    case "def"     => Some(DEF())
    case "var"     => Some(VAR())
    case "String"  => Some(STRING())
    case "extends" => Some(EXTENDS())
    case "Int"     => Some(INT())
    case "Bool"    => Some(BOOLEAN())
    case "while"   => Some(WHILE())
    case "if"      => Some(IF())
    case "else"    => Some(ELSE())
    case "return"  => Some(RETURN())
    case "length"  => Some(LENGTH())
    case "true"    => Some(TRUE())
    case "false"   => Some(FALSE())
    case "this"    => Some(THIS())
    case "new"     => Some(NEW())
    case "println" => Some(PRINTLN())
    case "do"      => Some(DO())

    case _         => None
  }

  private def isSpecChar(s: String): Option[Token] = s match {
    case ":"  => Some(COLON())
    case ";"  => Some(SEMICOLON())
    case "."  => Some(DOT())
    case ","  => Some(COMMA())
    case "==" => Some(EQUALS())
    case "="  => Some(EQSIGN())
    case "!"  => Some(BANG())
    case "("  => Some(LPAREN())
    case ")"  => Some(RPAREN())
    case "["  => Some(LBRACKET())
    case "]"  => Some(RBRACKET())
    case "{"  => Some(LBRACE())
    case "}"  => Some(RBRACE())
    case "&&" => Some(AND())
    case "||" => Some(OR())
    case "<"  => Some(LESSTHAN())
    case "+"  => Some(PLUS())
    case "-"  => Some(MINUS())
    case "*"  => Some(TIMES())
    case "/"  => Some(DIV())

    case _    => None
  }

  /**
   * Reads the contents of a file, caching two characters at a time.
   * That way we can have a 2-character lookahead with
   * currentChar and nextChar
   */
  private class SourceReader(f: File) {
    private val source = Source.fromFile(f)
    /** We use this character to mark the end of the input stream. */
    val EndOfFile: Char = java.lang.Character.MAX_VALUE

    private var currentChar_ : Char = _
    private var nextChar_ : Char = _
    private var currentPos_ : Positioned = _
    private var nextPos_ : Positioned = _

    /** The current character */
    def currentChar = currentChar_
    /** The next character */
    def nextChar = nextChar_
    /** The position of the current character */
    def currentPos = currentPos_

    private def readChar(): Char = if (source.hasNext) {
      source.next
    } else {
      EndOfFile
    }

    /**
     * Consumes a character from the input.
     * nextChar becomes currentChar,
     * nextChar points to the first unread character.
     */
    def consume() = {
      currentChar_ = nextChar_
      currentPos_ = nextPos_
      nextChar_ = readChar()
      nextPos_ = new Positioned {}.setPos(f, source.pos)
    }

    /** Consume n characters */
    def consume(n: Int): Unit = for (i <- 1 to n) consume()

    // To start, read the first two characters of the file
    consume(2)
  }

  def run(ctx: Context)(f: File): Iterator[Token] = {
    import ctx.reporter._

    val reader = new SourceReader(f)
    import reader._

    /** Gets rid of whitespaces and comments and calls readToken to get the next token. */
    @scala.annotation.tailrec
    def nextToken(): Token = {

      while (Character.isWhitespace(currentChar)) {
        consume()
      }
      if (currentChar == '/' && nextChar == '/') {
        consume(2)
        // Skip until EOL
        while (currentChar != '\n' && currentChar != '\r' && currentChar != EndOfFile) consume()
        nextToken()
      } else if (currentChar == '/' && nextChar == '*') {
       consume(2)
       
       while (!(currentChar == '*' && nextChar == '/') && currentChar != EndOfFile) {
          consume()
       }
       
       if(currentChar == EndOfFile) {
         error("Multi-line comment isn't closed before the end of the file")
       }
       consume(2)

        nextToken()
      } else {
        readToken()
      }
    }

    /** Reads the next token from the stream. */
    def readToken(): Token = {
      // The position at the beginning of the token.
      val tokenPos = currentPos
      
      if(currentChar == EndOfFile) {
        EOF().setPos(tokenPos)
      } else if(isSpecChar(currentChar.toString() + nextChar) != None) {
        isSpecChar(currentChar.toString() + nextChar) match {
          case Some(e) => {
            consume(2)
            e.setPos(tokenPos)
          }
          case None => throw new IllegalStateException
        }
      } else if(isSpecChar(currentChar.toString()) != None) {
        isSpecChar(currentChar.toString()) match {
          case Some(e) => {
            consume()
            e.setPos(tokenPos)
          }
          case None => throw new IllegalStateException
        }
      } else if(currentChar == '"') {
        
          def stringTok(str: String) : Token = {
            nextChar match {
              case '\n' => {
                error("String with multiple line")
                BAD().setPos(tokenPos)
              }
              case '\r' => {
                error("String with multiple line")
                BAD().setPos(tokenPos)
              }
              case '"' => {
                consume(2)
                STRINGLIT(str).setPos(tokenPos) 
              }
              case EndOfFile => {
                error("Reach end of file without closing string")
                EOF().setPos(tokenPos)
              }
              case _ => {
                consume()
                stringTok(str + currentChar)
              }
            }
          }
          
          nextChar match {
            case '"' => {
              consume(2)
              STRINGLIT("").setPos(tokenPos)
            }
            case _ => {
              consume()
              stringTok(currentChar.toString())  
            }
          }
          
      } else if(currentChar.isDigit) {
        
        def intTok(str: String) : Token = {
          if(nextChar.isDigit) {
            consume()
            intTok(str + currentChar)
          } else {
            consume()
            INTLIT(str.toInt).setPos(tokenPos)
          }
        }
        
        intTok(currentChar.toString())
      } else if(currentChar.isLetter) {
        
        def keyWIdTok(str: String) : Token = {
          if(nextChar.isLetterOrDigit || nextChar == '_') {
            consume()
            keyWIdTok(str + currentChar)
          } else {
            consume()
            keywords(str) match {
              case Some(e) => 
                e.setPos(tokenPos)
              case None =>
                ID(str).setPos(tokenPos)
            }
          }
        }
        
        keyWIdTok(currentChar.toString())
      } else {
        consume()
        error("Unknown character")
        BAD().setPos(tokenPos)
      }

    }
    
    new Iterator[Token] {
      var tokenCache: Token = nextToken()
      var reachedEnd = false

      def hasNext = !reachedEnd

      def next = {
        val r = tokenCache
        if (r == EOF()) {
          reachedEnd = true
        } else {
          tokenCache = nextToken()
        }
        r
      }
    }
  }
}

/** Reads and displays the tokens, then returns a fresh iterator with the same tokens. */
object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    val l = tokens.toList
    l foreach { t => println(s"$t(${t.line}:${t.col})") }
    l.iterator
  }
}
