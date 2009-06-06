package logic.reduction
import java.util.regex.Pattern
import logic.Node
import logic.Node._
import scala.collection.mutable.Map

object PredicateImplicits {
  class RichNode(node:Node){
    def isProposition = {
      node.getProperty("isProposition").asInstanceOf[Boolean]
    }
    def setInstantiated(bool:Boolean){
      node.setProperty("isInstantiated", bool.asInstanceOf[AnyRef])
    }
    def setVariable(variable:Char){
      node.setProperty("variable", variable.asInstanceOf[AnyRef])
    }
    def negated:Node = {
      val negatedNode = node.copy
      negatedNode.setProperty("negated", true.asInstanceOf[AnyRef])
      return negatedNode
    }
    def negate = negated
    def unary_! = negate
  }

  implicit def nodeToRichNode(node:Node):RichNode = {
    new RichNode(node)
  }
}

class PredicateReducer extends Reducer{
  import PredicateImplicits._
  def closed(node: Node, history:List[Node]): Boolean = {
    if(node.isProposition && history.contains(node.negated)){
      return true
    }
    else if(node.children.size == 0){
      return false
    }
    else {
      for(child <- node.children){
	if(!closed(child, history)){
	  return false
	}
      }
      return true
    }
  }

  def reduce(root: Node): Node = {
    root match {
      case Node("&", a, b) => Conjunction(a, b)
      case Node("v", a, b) => Disjunction(a, b)
      case Node("->", a, b) => Conditional(a, b)
      case Node("=", a, b) => Biconditional(a, b)
      case Node("~", Node("~", a)) => reduce(a)
      case Node("~", a) => reduce(!a)
      case Node(t) => Proposition(t) //assumes a proposition
      case Node(quant, a) => quantifier(quant, a)
      case _ => throw new RuntimeException("Pattern Not Recognized: " + root.lispify)
    }
  }

  def instantiate(node: Node, variable:Char):Node = {
    if(node.isProposition){
      val instantiatedNode = node.copy()
      instantiatedNode.setInstantiated(true)
      instantiatedNode.setVariable(variable)
      return instantiatedNode
    } else {
      return Node(node.text,
                  node.children.map(instantiate(variable, _)):_*)    
    }
  }

}
