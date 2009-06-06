package logic.reduction
import logic.parsing.SententialParser
import logic.parsing.PredicateParser
import logic.Node._
import scala.collection.mutable.ListBuffer
import scala.util.Sorting

abstract class CompoundReducer {
  def lispify(node: Node): String = {
    var retVal = "(" + node.text
    node.children.foreach(retVal += lispify(_))
    retVal += ")"
    return retVal
  }

  def parseSentences(premises:List[String], conclusion:String): Node = {
    val negatedConclusion = "~(" + conclusion + ")"
    return parseSentences(premises + negatedConclusion)
  }

  def parseSentences(sentences: List[String]): Node = {
    val trees = new ListBuffer[Node]()
    for(i <- 0 to sentences.size - 1)
      trees.append(parseSentence(sentences(i)))

    val treeArray = trees.toArray
    Sorting.quickSort(treeArray)

    var i = 1
    var node: Node = treeArray(0)
    while(i < treeArray.size){
      node = Node.stack(node, treeArray(i))
      i += 1
    }

    return node
  }

  def reduceSentences(premises:List[String], conclusion:String): Node = {
    val negatedConclusion = "~(" + conclusion + ")"
    return reduceSentences(premises + negatedConclusion)
  }

  def reduceSentences(sentences: List[String]): Node = {
    val trees = new ListBuffer[Node]()
    for(i <- 0 to sentences.size - 1)
      trees.append(reduceSentence(sentences(i)))

    val treeArray = trees.toArray
    Sorting.quickSort(treeArray)

    var i = 1
    var node: Node = treeArray(0)
    while(i < treeArray.size){
      node = Node.stack(node, treeArray(i))
      i += 1
    }

    return node
  }

  def reduceSentence(sentence: String): Node
  def parseSentence(sentence: String): Node

  def valid(node: Node):Boolean
  def invalid(node: Node):Boolean
}

class CompoundSententialReducer extends CompoundReducer{
  val sententialReducer = new SententialReducer

  def reduceSentence(sentence: String): Node = {
    sententialReducer.reduce(convertTextToAst(sentence))
  }

  def parseSentence(sentence: String): Node =
    new SententialParser(sentence).parse

  def convertTextToAst(sentence: String): Node = {
    return new SententialParser(sentence).parse
  }

  def valid(node: Node):Boolean = {
    sententialReducer.hasNoSolutions(node, List())
  }
  def invalid(node: Node):Boolean = {
    !sententialReducer.hasNoSolutions(node, List())
  }
}

class CompoundPredicateReducer extends CompoundReducer {
  val predicateReducer = new PredicateReducer

  def reduceSentence(sentence: String): Node = {
    return predicateReducer.reduce(convertTextToAst(sentence))
  }

  def parseSentence(sentence: String): Node =
    new PredicateParser(sentence).parse

  def convertTextToAst(sentence: String): Node = {
    return new PredicateParser(sentence).parse
  }

  def valid(node: Node):Boolean = {
    predicateReducer.hasNoSolutions(node, List())
  }

  def invalid(node: Node):Boolean = {
    !predicateReducer.hasNoSolutions(node, List())
  }
}

