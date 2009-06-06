package logic
import java.util.regex.Pattern

case class Node(text: String, children: Node*) extends Ordered[Node] {
  val convertedPropPattern = Pattern.compile("""~?[A-Z]([a-z]|[0-9]+|_[0-9]+)""")
  override def compare(that: Node) = this.size - that.size 
  
  def unary_! : Node = Node("~", Node(text, children: _*))

  def stringify: String = {
    if(children.size == 0)
      text
    else if(children.size == 1)
      text + children(0).stringify
    else
      Node.commonStringify(text, children(0), children(1))
  }

  override def toString: String = text

  def toString(prettifier: Prettifier) = {
    prettifier.prettify(text)
  }

  override def equals(that: Any): Boolean = false
  
  def size:Int = {
    val childSizes = children.map(_.size)
    if(childSizes.size != 0)
      return childSizes.reduceLeft(_ + _)
    else
      return 1
  }

  def lispify: String = {
    var retVal = ""
    if(children.size == 0){
      retVal = text
    }
    else{
      retVal = "("
      retVal += text
      retVal += " "
      var i = 0
      while(i < children.size){
        retVal += children(i).lispify
        if(i < children.size - 1) retVal += " "
        i += 1
      }
      retVal += ")"
    }
    retVal
  }
}

object Node{

  def stack(a: Node, b: Node): Node = {
    if(a == null && b == null)
      return null
    else if(a != null && b == null)
      return a
    else if(a == null && b != null)
      return b
    else if(a == b)
      return a
    else if(a.children.size != 0)
      Node(a.text, a.children.map(stack(_, b)): _*)
    else
      return Node(a.text, b)
  }
  
  def commonStringify(text: String, a: Node, b: Node): String = {
    var lead = ""
    if(text.indexOf("~") != -1)
      lead = "~("
    else
      lead = "("
 
    lead + a.stringify + text.replaceAll("~", "") + b.stringify + ")"
  }

  def max(a: Node, b: Node) = if(a.size > b.size) a else b

  def min(a: Node, b: Node) = if(a.size < b.size) a else b
}

trait Prettifier {
  def prettify(text: String): String
}

class PredicatePrettifier extends Prettifier {
  def prettify(text: String) = {
    "blah"
  }
}

												
