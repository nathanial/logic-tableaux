package logic
import logic.parsing.SententialLexer
import logic.parsing.SententialParser
import logic.reduction.SententialReducer
import logic.Node._
import scala.collection.mutable.ListBuffer

abstract class Reducer {
  def valid(node: Node) = hasNoSolutions(node, List())

  def invalid(node: Node) = !hasNoSolutions(node, List())

  def hasNoSolutions(node: Node, propositions: List[String]): Boolean

  def stackAnnotate(a:Node):Node = {
    stack(annotate(a), reduce(a))
  }

  def stackAnnotate(a:Node, b:Node):Node = {
    val max = Node.max(a, b)
    var min:Node = null
    if(max eq a)
      min = b
    else
      min = a

    stack(stack(annotate(min), annotate(max)), stack(reduce(min), reduce(max)))
  }

  def annotate(node: Node):Node

  def reduce(root: Node):Node

  case class Proposition(t: String) extends Node(t) {
    override def unary_! : Proposition = Proposition("~" + text)
  }

  case class Disjunction(a: Node, b: Node)
  extends Node("v", stackAnnotate(a), stackAnnotate(b)) {
    override def unary_! : Node = Conjunction(!a, !b)
    override def stringify: String = commonStringify(text, a, b)
  }

  case class Conjunction(a: Node, b: Node)
  extends Node("&", stackAnnotate(a, b)){
    override def unary_! : Node = Disjunction(!a, !b)
    override def stringify: String = commonStringify(text, a, b)
  }

  case class Conditional(a: Node, b: Node)
  extends Node("->", stackAnnotate(!a), stackAnnotate(b)){
    override def unary_! : Node = Conjunction(a, !b)
    override def stringify: String = commonStringify(text, a, b)
  }

  case class Biconditional(a: Node, b: Node)
  extends Node("=", stackAnnotate(a, b), stackAnnotate(!a, !b)){
    override def unary_! : Node = Biconditional(!a, b)
    override def stringify: String = commonStringify(text, a, b)
  }
}

