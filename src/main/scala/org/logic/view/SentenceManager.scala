package logic.view
import java.awt.Toolkit
import javax.swing.JFrame
import java.awt.FlowLayout
import javax.swing.JTextField
import javax.swing.JPanel
import javax.swing.BoxLayout
import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JComboBox
import javax.swing.JLabel
import javax.swing.JCheckBox
import java.awt.event.ItemEvent
import java.awt.event.ItemListener
import scala.collection.mutable.ListBuffer

class SentenceManager(view: InputView) {
  var conclusionPanel:JPanel = null
  val fields = new ListBuffer[JTextField]()
  val panel = new JPanel()
  panel setLayout new BoxLayout(panel, BoxLayout.Y_AXIS)

  def sentences: List[String] = List.fromArray(fields.map(_.getText).toArray)

 def addSentenceField {
    panel.removeAll()
    val newField = new JTextField(30)
    conclusionPanel = createConclusionPanel(newField)

    for(field <- fields){
      panel add field
    }
    panel add conclusionPanel
    fields += newField
    view.refresh
  }

  def removeSentenceField {
    panel remove conclusionPanel
    fields -= fields.last
    panel remove fields.last
    conclusionPanel = createConclusionPanel(fields.last)
    panel add conclusionPanel
    view.refresh
  }

 def createConclusionPanel(sentenceField: JTextField):JPanel = {
    val panel = new JPanel()
    panel setLayout new BoxLayout(panel, BoxLayout.X_AXIS)
    panel add new JLabel("|-")
    panel add sentenceField
    return panel
  }
}
