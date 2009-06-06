package logic.tests
import logic.parsing.SententialLexer
import logic.parsing.SententialParser
import logic.reduction.SententialReducer
import logic.reduction.CompoundSententialReducer

object TestSentential{
  val red = new CompoundSententialReducer
  val sententialReducer = new SententialReducer

  def run {
    testLexer
    testParser1
    batch1; batch2; batch3
    testDubNeg
  }

  def negate(sentence:String):String = "~(" + sentence + ")"

  def testDubNeg{
    println("testing double negation")
    val tree = new SententialParser(negate("P&~T")).parse
    val red = sententialReducer.reduce(tree)
    println(tree.lispify)
    println(red.lispify)
    println("end testing double negation")
  }

  def testLexer{
    println(SententialLexer.lex("((P=Q)&Q)->P"))
    println(SententialLexer.lex("((PvQ)&(~Pv~Q))->(~P=Q)"))
    println(SententialLexer.lex("((Pv~Q)&(Qv~P))->~(P=~Q)"))
    println(SententialLexer.lex("((P->(~Q&~R))&(~R=Q))->(Rv~P)"))
    println(SententialLexer.lex("(((PvQ)->R)&((RvS)->~T))->(P->~T)"))
    println(SententialLexer.lex("((A->B)&(B->C))->(A->C)"))
    println(SententialLexer.lex("((A=B)&(B=C))->(A=C)"))
  }

  def testParser1{
    println(new SententialParser(negate("((P=Q)&Q)->P")).parse.lispify)
    println(new SententialParser(negate("((PvQ)&(~Pv~Q))->(~P=Q)")).parse.lispify)
    println(new SententialParser(negate("((Pv~Q)&(Qv~P))->~(P=~Q)")).parse.lispify)
    println(new SententialParser(negate("((P->(~Q&~R))&(~R=Q))->(Rv~P)")).parse.lispify)
    println(new SententialParser(negate("(((PvQ)->R)&((RvS)->~T))->(P->~T)")).parse.lispify)
    println(new SententialParser(negate("((A->B)&(B->C))->(A->C)")).parse.lispify)
    println(new SententialParser(negate("((A=B)&(B=C))->(A=C)")).parse.lispify)
  }

  def batch1 {
    testValid("s2a", "((P=Q)&Q)->P")
    testValid("s3a", "((PvQ)&(~Pv~Q))->(~P=Q)")
    testValid("s4a", "((Pv~Q)&(Qv~P))->~(P=~Q)")
    testValid("s5a", "((P->(~Q&~R))&(~R=Q))->(Rv~P)")
    testValid("s6a", "(((PvQ)->R)&((RvS)->~T))->(P->~T)")
    testValid("s7a", "((A->B)&(B->C))->(A->C)")
    testValid("s8a", "((A=B)&(B=C))->(A=C)")
  }

  def batch2 {
    testInvalid("s1b", "(((P->~Q)&(~(PvQ)vP))&(~(P&Q)->(QvP)))->(~(P&Q)->~P)")
    testInvalid("s2b", "(((P->Q)v(Q->R))&(~R->~(P&Q)))->(Q->~P)")
    testInvalid("s3b", "((((PvQ)->(RvS))&(P=~(R&S)))&(Q=~(P&R)))->((S&P)->~(PvQ))")
    testInvalid("s4b", "((P=(Rv(P&~Q)))&(R=(Qv(R&~P))))->(Q=(Pv(Q&~R)))")
  }

  def batch3 {
    testValid("s1c", "(((A=B)&(B=C))->(A=C))&(((A->B)&(B->C))->(A->C))")
    testValid("s2c", "(((((P&~Q)->(R=~S))&~(SvZ))&(((~Q->R)->(TvW))&((W&~R)->T)))->(P->(~T->R)))")
  }

  def testValid(name: String, sentence: String){
    println("name begin = " + name)
    val tree = new SententialParser(negate(sentence)).parse
    val red = sententialReducer.reduce(tree)
    val isValid = sententialReducer.valid(red)
    assert(isValid)
    println("name end = " + name)
  }

  def testInvalid(name: String, sentence: String){
    println("name begin = " + name)
    val tree = new SententialParser(negate(sentence)).parse
    val red = sententialReducer.reduce(tree)
    val isInvalid = sententialReducer.invalid(red)
    assert(isInvalid)
    println("name end = " + name)
  }
}
