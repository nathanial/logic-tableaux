package logic.parsing
import java.util.regex.Pattern
import logic.Node
import logic.parsing.Parser._

/* NEG? ('('! sentence ')'! (OP^ sentence)? | PROP (OP^ sentence)?) */
/* sentence : (NEG? QUANT)* NEG? ('(' sentence ')' (OP^ sentence)? | PREDPROP (OP^ sentence)?) */

abstract class Parser(tokens: List[Token]) {
  var cursor = 0

  def test(target: String): Boolean = target.eq(currentToken.text)

  def test(pat: Pattern): Boolean = pat.matcher(currentToken.text).matches

  def getTokens = tokens

  def parse: Node = {
    return sentence
  }

  def consume(target: String):Token = {
    if(test(target))
      return nextToken
    else
      throw new RuntimeException("Consume failed on " + currentToken)
  }

  def consume(pat: Pattern):Token = {
    if(test(pat))
      return nextToken
    else
      throw new RuntimeException("Consume failed on " + currentToken)
  }

  def nextToken: Token = {
    val token = currentToken
    cursor = cursor + 1
    token
  }

  def currentToken: Token = {
    if(cursor < tokens.size)
      tokens(cursor)
    else
      Token("","")
  }

  def obtainToken(i: Int): Token = {
    if(i < tokens.size)
      tokens(i)
    else
      Token("", "")
  }

  def modifiers:List[String] = {
    if(currentToken.text.eq("NEG") && obtainToken(cursor + 1).text.eq("QUANT"))
      consume("NEG").value :: consume("QUANT").value :: modifiers
    else if(currentToken.text.eq("QUANT"))
      consume("QUANT").value :: modifiers
    else if(currentToken.text.eq("NEG")){
      consume("NEG").value :: modifiers
    }
    else
      List()
  }

  def sentence:Node = {
    val modList = modifiers

    if(test("L_PAREN")){
      consume("L_PAREN")
      var sen = sentence
      consume("R_PAREN")

      sen = stackModifiers(sen, modList)

      val tuple = secondHalf
      if(tuple == null)
        return sen
      else
        return Node(tuple._1, sen, tuple._2)
    }
    else if(test("PROP")){
      val prop = stackModifiers(Node(consume("PROP").value), modList)

      val tuple = secondHalf
      if(tuple == null)
        return prop
      else
        return Node(tuple._1, prop, tuple._2)
    }
    else{
      throw new RuntimeException("Sentence Failed on " + currentToken)
    }
  }

  def secondHalf = {
    if(test("OP"))
      (consume("OP").value, sentence)
    else
      null
  }

  def stackModifiers(node: Node, quantList:List[String]):Node = {
    if(quantList.size > 0)
      Node(quantList.head, stackModifiers(node, quantList.tail))
    else
      node
  }
}

class SententialParser(t: String) extends Parser(SententialLexer.lex(t))
class PredicateParser(text:String) extends Parser(PredicateLexer.lex(text)){
  override def parse: Node = {
    val node = super.parse
    if(hasConflictingQuantifiers(node, List[String]()))
      throw new RuntimeException("Conflicting quantifiers found in sentence")
    else
      return node
  }
  def hasConflictingQuantifiers(node: Node, quantifiers: List[String]):Boolean = {
    var newQuantifiers = quantifiers
    if(isQuantifier(node.text))
      newQuantifiers = node.text :: quantifiers

    if(quantifierCollision(newQuantifiers))
      return true
    else{
      val badBranch = node.children.find(hasConflictingQuantifiers(_, newQuantifiers))
      return badBranch != None
    }
  }

  def getVar(quant: String) = quant.charAt(2)

  def isQuantifier(text: String) = uniPat.matcher(text).matches || exiPat.matcher(text).matches

  def quantifierCollision(quantifiers: List[String]): Boolean = {
    !quantifiers.forall(x => {
      None == quantifiers.find(y => { x != y && getVar(x) == getVar(y)})
    })
  }
}

object Parser{
  val uniPat = Pattern.compile("""A\([a-z]\)""")
  val exiPat = Pattern.compile("""E\([a-z]\)""")
}
