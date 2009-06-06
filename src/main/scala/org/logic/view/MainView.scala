package logic.view
import logic.reduction.CompoundSententialReducer
import logic.reduction.CompoundPredicateReducer
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

object InputView {
  def framePosition(frame: JFrame):(Int, Int) = {
    val toolkit = Toolkit.getDefaultToolkit
    val screenSize = toolkit.getScreenSize
    val x:Int = ((screenSize.getWidth - frame.getWidth) / 2).toInt
    val y:Int = ((screenSize.getHeight - 600) / 2).toInt
    return (x, y)
  }

  def topFrame:JFrame = {
    val frame = new JFrame("Formula Input")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.getContentPane.setLayout(new FlowLayout)
    val (x, y) = framePosition(frame)
    frame.setLocation(x, y)
    return frame
  }
}

class InputView(grapher: Grapher, graphView: GraphView){
  val validLabel = new JLabel()
  val controller = new InputController(grapher, graphView, this)
  val frame = InputView.topFrame
  val sententialMode = true
  val sentenceManager = new SentenceManager(this)
  val buttonManager = new ButtonManager(this)

  frame.getContentPane add sentenceManager.panel
  frame.getContentPane add buttonManager.panel

  frame.pack
  frame setVisible true

  sentenceManager.addSentenceField
  sentenceManager.addSentenceField

  def refresh {
    sentenceManager.panel.repaint()
    sentenceManager.panel.revalidate()
    frame.pack
    frame.repaint()
  }

}

class SListener(f:(ActionEvent) => Unit) extends ActionListener {
  override def actionPerformed(e: ActionEvent) = f(e)
}
class SItemListener(f:(ItemEvent) => Unit) extends ItemListener {
  override def itemStateChanged(e: ItemEvent) = f(e)
}

class GraphView(grapher: Grapher) {
  val graphView = new JFrame("Graph View")
  graphView.setSize(600, 600)
  graphView.getContentPane add grapher
  graphView setVisible true

  val toolkit = Toolkit.getDefaultToolkit
  val screenSize = toolkit.getScreenSize
  val x = (screenSize.getWidth - 600) / 2
  val y = (screenSize.getHeight - 600) / 2

  graphView.setLocation((x - 300).toInt, y.toInt)

  def refresh{
    graphView.pack
    graphView.repaint()
  }
}

object Main extends Application {
  val grapher = new Grapher
  new InputView(grapher, new GraphView(grapher))
}
