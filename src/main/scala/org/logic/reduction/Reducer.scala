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
}

