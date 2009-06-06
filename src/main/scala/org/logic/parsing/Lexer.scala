package logic.parsing
import java.util.regex.Pattern

abstract class Lexer{

  def lex(text: String): List[Token]
  
  def test(target: String, lookahead: Int, text: String): Boolean = {
    val yt = text.take(lookahead).toString
    val ret = target == yt
    return ret
  }
  
  def test(pat: Pattern, lookahead: Int, text: String): Boolean = {
    val yt = text.take(lookahead).toString
    val ret = pat.matcher(yt).matches
    return ret
  }
}

case class Token(text: String, value: String)

object PredicateLexer extends Lexer{
  val propPat = Pattern.compile("""[A-Z][a-z]""")
  val opPat1 = Pattern.compile("""(&|v|=)""")
  val cond = "->"
  val quant = Pattern.compile("""(A|E)\([a-z]\)""")

  override def lex(text: String):List[Token] = {
    if(test("~~", 2, text))
      lex(text drop 2)
    else if(test(" ", 1, text))
      lex(text drop 1)
    else if(test("~", 1, text))
      Token("NEG", "~") :: lex(text drop 1)
    else if(test(propPat, 2, text))
      Token("PROP", text take 2) :: lex(text drop 2)
    else if(test(quant, 4, text))
      Token("QUANT", text take 4) :: lex(text drop 4)
    else if(test(opPat1, 1, text))
      Token("OP", text take 1) :: lex(text drop 1)
    else if(test(cond, 2, text))
      Token("OP", text take 2) :: lex(text drop 2)
    else if(test("(", 1, text))
      Token("L_PAREN", text take 1) :: lex(text drop 1)
    else if(test(")", 1, text))
      Token("R_PAREN", text take 1) :: lex(text drop 1)
    else
      List()
  }
}

object SententialLexer extends Lexer {
  import logic.parsing.SententialLexer._

  val propPat = Pattern.compile("""[A-Z]""")
  val opPat1 = Pattern.compile("""(&|v|=)""")
  val cond = "->"

  override def lex(text: String):List[Token] = {
    if(test("~~", 2, text))
      lex(text drop 2)
    else if(test(" ", 1, text))
      lex(text drop 1)
    else if(test("~", 1, text))
      Token("NEG", "~") :: lex(text drop 1)
    else if(test(propPat, 1, text))
      Token("PROP", text take 1) :: lex(text drop 1)
    else if(test(opPat1, 1, text))
      Token("OP", text take 1) :: lex(text drop 1)
    else if(test(cond, 2, text))
      Token("OP", text take 2) :: lex(text drop 2)
    else if(test("(", 1, text))
      Token("L_PAREN", text take 1) :: lex(text drop 1)
    else if(test(")", 1, text))
      Token("R_PAREN", text take 1) :: lex(text drop 1)
    else
      List()
  }
}
