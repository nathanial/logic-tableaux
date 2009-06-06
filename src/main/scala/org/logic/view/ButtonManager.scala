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


class ButtonManager(view: InputView) {
  val engageButton = createEngageButton
  val addSentenceButton = createAddButton
  val removeSentenceButton = createRemoveButton
  val logicModeBox = createLogicModeBox
  val astSwitch = createAstSwitch
  val manPanel = createManPanel
  val topPanel = createTopPanel
  val panel = createControlPanel

  def createManPanel:JPanel = {
    val manPanel = new JPanel()
    manPanel.setLayout(new FlowLayout)
    manPanel add addSentenceButton
    manPanel add removeSentenceButton
    manPanel add engageButton
    return manPanel
  }

  def createControlPanel:JPanel = {
    val controlPanel = new JPanel
    controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.Y_AXIS))
    controlPanel add topPanel
    controlPanel add manPanel
    return controlPanel
  }

  def createTopPanel:JPanel = {
    val topPanel = new JPanel
    topPanel.setLayout(new FlowLayout)
    topPanel add view.validLabel
    topPanel add logicModeBox
    topPanel add astSwitch
    return topPanel
  }

  def createEngageButton:JButton = {
    val button = new JButton("Engage")
    button.addActionListener(
      new SListener(e => view.controller.setSentences(view.sentenceManager.sentences)))
    button.setToolTipText("Click to display graph")
    return button
  }

  def createAddButton:JButton = {
    val button = new JButton("+")
    button.addActionListener(new SListener(e => view.sentenceManager.addSentenceField))
    return button
  }

  def createRemoveButton:JButton = {
    val button = new JButton("-")
    button.addActionListener(new SListener(e => view.sentenceManager.removeSentenceField))
    return button
  }

  def createLogicModeBox:JComboBox = {
    val box = new JComboBox()
    box addItem "Sentential"
    box addItem "Predicate"
    box addActionListener new SListener(
      e => {
        val cb = e.getSource
        val mode = cb.asInstanceOf[JComboBox].getSelectedItem
        if(mode == "Sentential")
          view.controller.sententialMode = true
        else if(mode == "Predicate")
          view.controller.sententialMode = false
        else
          throw new RuntimeException("Logic Mode Box Selection Not Recognize!")
      })
    box.setToolTipText("Convert between predicate and sentential logic modes")
    return box
  }

  def createAstSwitch: JCheckBox = {
    val astSwitch = new JCheckBox("AST")
    astSwitch.setSelected(false)
    astSwitch addItemListener new SItemListener(
      e => {
        if(e.getStateChange == ItemEvent.DESELECTED)
          view.controller.astMode = false
        else
          view.controller.astMode = true
      })
    astSwitch setToolTipText "Displays Abstract Syntax Tree"
    return astSwitch
  }

}
