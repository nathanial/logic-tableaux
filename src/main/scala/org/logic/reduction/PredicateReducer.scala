package logic.reduction
import java.util.regex.Pattern
import logic.Node
import logic.Node._
import scala.collection.mutable.Map

class PredicateReducer extends Reducer{
  val propPattern = Pattern.compile("""~?[A-Z][a-z]""")
  val convertedPropPattern = Pattern.compile("""~?[A-Z]([a-z]|[0-9]+|_[0-9]+)""")
  val uniPat = Pattern.compile("""A\([a-z]\)""")
  val exiPat = Pattern.compile("""E\([a-z]\)""")
  val capital = Pattern.compile("""[A-Z]""")

  def reset {
    ExistentialQuantifier.number = 0
  }

  def hasNoSolutions(node: Node, propositions: List[String]): Boolean = {
    return hasNoSolutions(node, propositions, Map[Int, String]())
  }

  //instantiationContext represents a map from Universal Quantifer counts to instance variables
  def hasNoSolutions(node: Node, propositions: List[String], instantiationContext: Map[Int, String]): Boolean = {
    val children = node.children
    var newPropositions = propositions

    if(convertedPropPattern.matcher(node.text).matches)
      newPropositions = node.text :: propositions

    if(inconsistent(newPropositions, instantiationContext))
      return true
    else if(children.size == 0)
      return false
    else
        for(child <- children){
          if(!hasNoSolutions(child, newPropositions, instantiationContext))
            return false
        }

    return true
  }

  def inconsistent(propositions: List[String], instantiationContext: Map[Int, String]) : Boolean = {
    for(prop <- propositions){
      val inconsistent = propositions.exists(inconsistentPropositions(instantiationContext, prop, _))
      if(inconsistent)
        return true
    }
    return false
  }

  def inconsistentPropositions(insCon: Map[Int, String], a:String, b:String): Boolean = {
    val trip1 = divideProposition(a)
    val trip2 = divideProposition(b)

    var instance1 = trip1.instance
    var instance2 = trip2.instance

    if(insCon.isDefinedAt(trip1.count)){
      instance1 = insCon(trip1.count)
    }
    else if(trip1.instance == "_" && !(trip2.instance == "_")){
      insCon.put(trip1.count, trip2.instance)
      instance1 = trip2.instance
    }

    if(insCon.isDefinedAt(trip2.count)){
      instance2 = insCon(trip2.count)
    }
    else if(trip2.instance == "_" && !(trip1.instance == "_")){
      insCon.put(trip2.count, trip1.instance)
      instance2 = trip1.instance
    }

    if(trip1.predicate == trip2.predicate &&
       (instance1 == instance2 || instance1 == "_" || instance2 == "_")){
         val opposites = (trip1.negated != trip2.negated)
         return opposites
       }
    else {
      return false
    }
  }

  def divideProposition(text: String): PredicateQuad = {
    val idx = text.findIndexOf((a: Char) => capital.matcher(a.toString).matches)
    val negated = text.indexOf("~") != -1
    val predicate = text.charAt(idx)

    var instance = ""
    var count = -1

    if(text.indexOf("_") != -1){ //is Universal Quantifier
      instance = "_"
      count = Integer.parseInt(text.substring(text.indexOf("_") + 1))
    }
    else{
      instance = text.substring(idx + 1)
    }

    return PredicateQuad(negated, predicate, instance, count)
  }

  case class PredicateQuad(negated: boolean, predicate: char, instance: String, count: Int)

  def annotate(node: Node):Node = {
 //   if(!propPattern.matcher(node.text).matches)
 //     Node(node.stringify)
 //   else
      null
  }

  //poor performance because of !, not meant for large trees
  def reduce(root: Node): Node = {
    root match {
    case Node("&", a, b) => Conjunction(a, b)
    case Node("v", a, b) => Disjunction(a, b)
    case Node("->", a, b) => Conditional(a, b)
    case Node("=", a, b) => Biconditional(a, b)
    case Node("~", Node("~", a)) => reduce(a)
    case Node("~", a) => !reduce(a)
    case Node(t) => Proposition(t) //assumes a proposition
    case Node(quant, a) => quantifier(quant, a)
    case _ => throw new RuntimeException("Pattern Not Recognized: " + root.lispify)
    }
  }

  def quantifier(quant: String, a: Node): Node = {
    if(uniPat.matcher(quant).matches)
      return UniversalQuantifier(getVar(quant), a)
    else if(exiPat.matcher(quant).matches)
      return ExistentialQuantifier(getVar(quant), a)
    else
      throw new RuntimeException("Unknown modifier = " + quant)
  }

  def getVar(quant: String) = quant.charAt(2)

  case class UniversalQuantifier(v: Char, a: Node)
  extends Node("A("+v+")" , reduce(UniversalQuantifier.instantiate(v, a))){
    UniversalQuantifier.count += 1
    override def unary_! : Node = ExistentialQuantifier(v, !a)
  }

  case class ExistentialQuantifier(v: Char, a: Node)
  extends Node("E("+v+")", reduce(ExistentialQuantifier.instantiate(v, a))){
    ExistentialQuantifier.number += 1
    override def unary_! : Node = UniversalQuantifier(v, !a)
  }

  object UniversalQuantifier{
    var count:Int = 0

    def instantiate(variable: Char, node: Node): Node = {
      if(PredicateUtils.isProposition(node.text))
        return Node(uiText(node.text, variable),
                    node.children.map(instantiate(variable, _)):_*)
      else
        return Node(node.text,
                    node.children.map(instantiate(variable, _)):_*)
    }

    def uiText(propText: String, variable: Char):String = {
      val propVariable = propText.last
      if(propVariable == variable)
        return (propText.slice(0, propText.size - 1) + "_" + count)
      else
        return propText
    }
}

object ExistentialQuantifier{
  var number = 0

  def instantiate(variable: Char, node: Node): Node = {
    if(PredicateUtils.isProposition(node.text))
      return Node(eiText(node.text, variable),
                  node.children.map(instantiate(variable, _)):_*)
    else
      return Node(node.text,
                  node.children.map(instantiate(variable, _)):_*)
  }

  def eiText(propText: String, variable: Char):String = {
    val propVariable = propText.last
    if(propVariable == variable)
      return (propText.slice(0, propText.size - 1) + number)
    else
      return propText
  }

}

}
