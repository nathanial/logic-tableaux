package logic.view
import logic.reduction.CompoundSententialReducer
import logic.reduction.CompoundPredicateReducer
import javax.swing.JOptionPane
import javax.swing.JFrame

class InputController(grapher: Grapher, graphView: GraphView, inputView: InputView){
  var sententialMode = true
  var astMode = false
  val senRed = new CompoundSententialReducer
  val predRed = new CompoundPredicateReducer

  def setSentences(sentences: List[String]){
    val validSentences = sentences.filter(_.trim != "")
    val conclusion = validSentences.last
    val premises = validSentences - conclusion
    var tree: Node = null
    var valid = false

    try{
      if(sententialMode){
        println("SENTENTIAL MODE")
        if(!astMode)
          tree = senRed.reduceSentences(premises, conclusion)
        else
          tree = senRed.parseSentences(premises, conclusion)
        valid = senRed.valid(tree)
      }
      else{
        println("PREDICATE MODE")
        if(!astMode)
          tree = predRed.reduceSentences(premises, conclusion)
        else
          tree = predRed.parseSentences(premises, conclusion)
        valid = predRed.valid(tree)
      }
    } catch {
      case ex: RuntimeException => JOptionPane.showMessageDialog(null, ex.getMessage)
    }

    if(astMode)
      inputView.validLabel setText "N/A"
    else if(valid)
      inputView.validLabel setText "Valid"
    else
      inputView.validLabel setText "Invalid"

    inputView.refresh

    grapher showGraph tree
    graphView.refresh
  }
}
