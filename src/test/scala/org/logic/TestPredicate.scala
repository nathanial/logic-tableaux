package logic.tests
import logic.parsing.PredicateLexer
import logic.parsing.PredicateParser
import logic.reduction.PredicateReducer

object TestPredicate{
  val predicateReducer = new PredicateReducer

  def run{
    testLexer
    testParser
    testParser2
    testValidity
  }

  def negate(sentence:String):String = "~(" + sentence + ")"
  
  def testValidity{
    testPredicateInvalid("p1", "~A(x)(Ax->Bx)")
    testPredicateInvalid("p2", "E(x)(Ax->Bx)")
    testPredicateInvalid("p3", "E(x)Ax v E(x)Bx")
    testPredicateInvalid("p4", "A(x)((Px&((Rx&Fx)v(Ax&Yx)))->(GxvBx))")
    testPredicateInvalid("p5", "A(x)(Fx->Gx)&~(E(x)Gx v E(x)Hx)")
    testPredicateInvalid("p6", "~A(x)~((~Fx v Gx)->Hx) & (A(x)Hx -> A(x)Sx)")
    testPredicateInvalid("p7", "(A(x)Fx v A(x)Gx)&A(x)(Fx->Hx)")
    testPredicateInvalid("p8", "~A(x)E(y)(Ax->By) v E(x)Cx")
    testPredicateValid("p9", "E(x)(Ax v ~Ax)")
    testPredicateValid("p10", "(A(x)(Cx->Dx)&A(x)(Ex->~Dx))->A(x)(Ex->~Cx)")
    testPredicateValid("p11", "((~E(x)((Ax&Bx)&~Cx)&~E(x)(Ax&~Bx))&A(x)(Cx->~(Sx v Tx)))->~E(x)(Ax&Tx)")
    testPredicateInvalid("p12", "A(x)(Ax=Bx)->(Aa=Bc)")
    testPredicateValid("p13", "A(x)(Ax->Bc)->(Ac->Bc)")
    testPredicateValid("p14", "A(x)(Ab=Ad)->(Ab->Ad)")
    testPredicateInvalid("p15", "E(x)(Ax & Bc)->(Ac & Bc)")
    testPredicateValid("p16", "(A(x)(Ax->By)&A(x)(By->Cx))->A(x)(Ax->Cx)")
  }
  
  def testParser {
    println(new PredicateParser("A(x)(Ax->Bx)").parse.lispify)
    println(new PredicateParser("A(x)((Px&((Rx&Fx)v(Ax&Yx)))->(GxvBx))").parse.lispify)
    println(new PredicateParser("A(x)(Fx->Gx)&~(E(x)Gx v E(x)Hx)").parse.lispify)
    println(new PredicateParser("~A(x)~((~Fx v Gx)->Hx) & (A(x)Hx -> A(x)Sx)").parse.lispify)
    println(new PredicateParser("(A(x)Fx v A(x)Gx)&A(x)(Fx->Hx)").parse.lispify)
  }
  def testParser2{
    var fails = true
    try{
      println(new PredicateParser("A(x)(Ax->(Bx->E(x)Cx))").parse.lispify)
      fails = false
    }catch{
      case _ => 
    }
    assert(fails)
  }
  
  def testLexer {
    println(PredicateLexer.lex("A(x)(Ax->Bx)"))
    println(PredicateLexer.lex("A(x)((Px&((Rx&Fx)v(Ax&Yx)))->(GxvBx))"))
  }

  def testPredicateValid(name:String, sentence:String){
    val tree = new PredicateParser(negate(sentence)).parse
    val red = predicateReducer.reduce(tree)
    
    val isValid = predicateReducer.valid(red)// && !valid(tree)
    println(name + " = " + red.lispify)
    assert(isValid)
  }
  
  def testPredicateInvalid(name:String, sentence:String){
    val tree = new PredicateParser(negate(sentence)).parse
    val red = predicateReducer.reduce(tree)
 
    val isInvalid = predicateReducer.invalid(red)// && !valid(tree)
    assert(isInvalid)
  }
}

  
