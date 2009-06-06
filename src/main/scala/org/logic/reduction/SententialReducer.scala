package logic.reduction
import logic.Node._
import java.util.regex.Pattern

class SententialReducer extends Reducer{
  val propPattern = Pattern.compile("""~?[A-Z]""")

  def inconsistent(propositions: List[String]) : Boolean = {
    for(prop <- propositions)
      if(prop.indexOf("~") != -1 && propositions.contains(prop.replaceAll("~", "")))
        return true
      else if(propositions.contains("~" + prop))
        return true

    return false
  }

  def annotate(node: Node):Node = {
    if(!propPattern.matcher(node.text).matches)
      if(node.text != "~")
        Node(node.stringify)
      else if(!propPattern.matcher(node.children(0).text).matches)
        Node(node.stringify)
      else
        null
    else
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
    case _ => throw new RuntimeException("Pattern Not Recognized: " + root.toString)
    }
  }

  def hasNoSolutions(node: Node, propositions: List[String]): Boolean = {
    val children = node.children
    var newPropositions = propositions

    if(propPattern.matcher(node.text).matches)
      newPropositions = node.text :: propositions

    if(inconsistent(newPropositions))
      return true
    else if(children.size == 0)
      return false
    else
        for(child <- children)
          if(!hasNoSolutions(child, newPropositions))
            return false

    return true
  }
}


