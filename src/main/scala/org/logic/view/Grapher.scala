package logic.view

import java.awt.BorderLayout
import java.awt.Color._
import java.awt.Container
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.GridLayout
import java.awt.Shape
import java.awt.Paint
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.ItemEvent
import java.awt.event.ItemListener
import java.awt.geom.Ellipse2D
import java.awt.geom.Point2D
import java.util.Collection
import java.util.HashSet
import java.util.Map
import java.util.Set

import javax.swing.BorderFactory
import javax.swing.JApplet
import javax.swing.JButton
import javax.swing.JComboBox
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JToggleButton

import org.apache.commons.collections15.Factory
import org.apache.commons.collections15.functors.ConstantTransformer
import org.apache.commons.collections15.Transformer

import edu.uci.ics.jung.algorithms.layout.PolarPoint
import edu.uci.ics.jung.algorithms.layout.RadialTreeLayout
import edu.uci.ics.jung.algorithms.layout.TreeLayout
import edu.uci.ics.jung.graph.DirectedGraph
import edu.uci.ics.jung.graph.DirectedSparseMultigraph
import edu.uci.ics.jung.graph.Forest
import edu.uci.ics.jung.graph.SparseForest
import edu.uci.ics.jung.graph.SparseTree
import edu.uci.ics.jung.graph.Tree
import edu.uci.ics.jung.visualization.GraphZoomScrollPane
import edu.uci.ics.jung.visualization.Layer
import edu.uci.ics.jung.visualization.VisualizationServer
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.CrossoverScalingControl
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import edu.uci.ics.jung.visualization.control.ModalGraphMouse
import edu.uci.ics.jung.visualization.control.ScalingControl
import edu.uci.ics.jung.visualization.decorators.EdgeShape
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.layout.LayoutTransition
import edu.uci.ics.jung.visualization.util.Animator
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import edu.uci.ics.jung.visualization.decorators.InterpolatingVertexSizeTransformer
import edu.uci.ics.jung.visualization.decorators.AbstractVertexShapeTransformer

import logic.Node
import logic.reduction.CompoundSententialReducer
import logic.parsing.PredicateParser
import logic.reduction.PredicateReducer
import java.util.regex.Pattern


class PaintTransformer extends Transformer[Node, Paint] {
  override def transform(i: Node): Paint = white
}

class VertexSize extends AbstractVertexShapeTransformer[Node] with Transformer[Node, Shape]{
  setSizeTransformer(new Transformer[Node, Integer]{
                       override def transform(node: Node) = 10
                     })
  
  override def transform(node: Node): Shape = {
    return factory.getEllipse(node)
  }
}

class Grapher extends JApplet {
    val predicateReducer = new PredicateReducer

    val opPattern = Pattern.compile("""(&|v|->|=)""")

    val red = new CompoundSententialReducer

    val edgeFactory = new Factory[Integer]{
      var i = 0 
      override def create:Integer = {
        i += 1
        return i - 1
      }
    }

  var graph: Forest[Node,Integer] = null
    
  def negate(sentence:String) = ("~(" + sentence + ")")

  def showGraph(rootNode: Node){
    getContentPane.removeAll
    graph = null

    var vv: VisualizationViewer[Node, Integer] = null
    var root:String = null
    var treeLayout: TreeLayout[Node, Integer] = null

    graph = new SparseForest[Node,Integer]
    graph(rootNode)
    treeLayout = new TreeLayout[Node,Integer](graph)
    vv =  new VisualizationViewer[Node,Integer](treeLayout, new Dimension(600,600))
    vv.setBackground(white)
    vv.getRenderContext.setEdgeShapeTransformer(new EdgeShape.Line())
    vv.getRenderContext.setVertexLabelTransformer(new ToStringLabeller())
    vv.getRenderContext.setVertexFillPaintTransformer(new PaintTransformer)
    vv.getRenderContext.setVertexShapeTransformer(new VertexSize)

    vv.setVertexToolTipTransformer(new ToStringLabeller())
    vv.getRenderer.getVertexLabelRenderer.setPosition(Position.E)

    val content = getContentPane
    val panel = new GraphZoomScrollPane(vv)
    content.add(panel)

    val graphMouse = new DefaultModalGraphMouse()

    vv.setGraphMouse(graphMouse)

    graphMouse.setMode(ModalGraphMouse.Mode.TRANSFORMING)

    val scaler = new CrossoverScalingControl

    val plus = new JButton("+")
    plus.addActionListener(new ActionListener() {
         override def actionPerformed(e: ActionEvent) {
              scaler.scale(vv, 1.1f, vv.getCenter())
         }
    })
    val minus = new JButton("-")
    minus.addActionListener(new ActionListener() {
		override def actionPerformed(e: ActionEvent) {
			scaler.scale(vv, 1/1.1f, vv.getCenter())
        }
    })


    val scaleGrid = new JPanel(new GridLayout(1,0))
    scaleGrid.setBorder(BorderFactory.createTitledBorder("Zoom"))

    val controls = new JPanel()
    scaleGrid.add(plus)
    scaleGrid.add(minus)
    controls.add(scaleGrid)

    content.add(controls, BorderLayout.SOUTH)
  }

  def graph(root: Node){
    if(!opPattern.matcher(root.text).matches || root.children.size > 1){
      graph.addVertex(root)
      root.children.foreach(createTree(_, root))
    }
    else{
      val child = root.children(0)
      graph.addVertex(child)
      child.children.foreach(createTree(_, child))
    }
  }

  def createTree(current: Node, previous: Node){
    val newNode = Node(current.text, current.children:_*)
    if(!opPattern.matcher(newNode.text).matches || newNode.children.size > 1){
      graph.addEdge(edgeFactory.create(), previous, newNode)
      newNode.children.foreach(createTree(_, newNode))
    }
    else{
      newNode.children.foreach(createTree(_, previous))
    }
  }
 
}
