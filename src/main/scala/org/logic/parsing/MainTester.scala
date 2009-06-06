package logic.parsing

object MainTester {
  def main(args : Array[String]) : Unit = {
    println("blah")
    test1
    test2
    test3
    1
  }
  
  def test1 {
    println(new SententialParser("((P=Q)&Q)->P").sentence)    
    println(new SententialParser("((PvQ)&(~Pv~Q))->(~P=Q)").sentence)
    println(new SententialParser("((Pv~Q)&(Qv~P))->~(P=~Q)").sentence)
    println(new SententialParser("((P->(~Q&~R))&(~R=Q))->(Rv~P)").sentence)
    println(new SententialParser("(((PvQ)->R)&((RvS)->~T))->(P->~T)").sentence)
    println(new SententialParser("((A->B)&(B->C))->(A->C)").sentence)
    println(new SententialParser("((A=B)&(B=C))->(A=C)").sentence)
  }
  
  def test2 {
    println(new SententialParser("(((P->~Q)&(~(PvQ)vP))&(~(P&Q)->(QvP)))->(~(P&Q)->~P)").sentence)
    println(new SententialParser("(((P->Q)v(Q->R))&(~R->~(P&Q)))->(Q->~P)").sentence)
    println(new SententialParser("((((PvQ)->(RvS))&(P=~(R&S)))&(Q=~(P&R)))->((S&P)->~(PvQ))").sentence)
    println(new SententialParser("((P=(Rv(P&~Q)))&(R=(Qv(R&~P)))) -> (Q=(Pv(Q&~R)))").sentence)
  }
  
  def test3 {
    println(new SententialParser("(((A=B)&(B=C))->(A=C)) & (((A->B)&(B->C))->(A->C))").sentence)
    println(new SententialParser("(((((P&~Q)->(R=~S))&~(SvZ))&(((~Q->R)->(TvW))&((W&~R)->T)))->(P->(~T->R)))").sentence)
  }
  
  def failTest1{
    new SententialParser("((P==Q)&Q)->P").sentence    
    new SententialParser("((PvQ)&&(~Pv~Q))->(~P=Q)").sentence
    new SententialParser("((Pv~Q)&(Qv~PP))->~(P=~Q)").sentence
    new SententialParser("((P->(~Q&~Rx))&(~R=Q))->(Rv~P)").sentence
    new SententialParser("(((PvQ)->RA)&((RvS)->~T))->(P->~T)").sentence
    new SententialParser("((A->B)&(B-->C))->(A->C)").sentence
    new SententialParser("((A=B)&(B=CC))->(A=C)").sentence
  }
  def failTest2{
    
  }
  def failTest3{
    
  }
}
