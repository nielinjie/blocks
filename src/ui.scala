package nielinjie.app.blocks
package ui

import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import edu.uci.ics.jung.visualization.BasicVisualizationServer
import org.apache.commons.collections15.Transformer
import java.awt.Paint
import edu.uci.ics.jung.algorithms.layout.Layout
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.graph.UndirectedSparseGraph
import java.awt.Color
import java.awt.Dimension
import scala.swing.Component
import edu.uci.ics.jung.algorithms.layout.SpringLayout
import edu.uci.ics.jung.graph.DirectedSparseGraph
import edu.uci.ics.jung.algorithms.layout.KKLayout
import edu.uci.ics.jung.algorithms.layout.SpringLayout2
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import edu.uci.ics.jung.visualization.picking.ShapePickSupport
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.control.PluggableGraphMouse
import edu.uci.ics.jung.visualization.control.TranslatingGraphMousePlugin
import edu.uci.ics.jung.visualization.control.ScalingGraphMousePlugin
import edu.uci.ics.jung.visualization.control.PickingGraphMousePlugin
import edu.uci.ics.jung.visualization.control.CrossoverScalingControl
import edu.uci.ics.jung.algorithms.filters.VertexPredicateFilter
import org.apache.commons.collections15.Predicate
import java.awt.event.MouseEvent
import java.awt.Event
import nielinjie.util.io.Serializer
import nielinjie.util.io.FileUtil
import java.io.File
import edu.uci.ics.jung.algorithms.layout.FRLayout2
import edu.uci.ics.jung.algorithms.layout.FRLayout
import edu.uci.ics.jung.algorithms.layout.ISOMLayout
import scala.swing.Panel
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position._
import scala.swing.Label
import java.awt.event.ItemListener
import java.awt.event.ItemEvent
import scalaz._
import Scalaz._
import scala.swing.FlowPanel
import scala.swing.Button
import scala.swing.event.ButtonClicked
import edu.uci.ics.jung.algorithms.filters.Filter
import domain._
import edu.uci.ics.jung.graph.util.Context
import scala.collection.mutable.ArrayBuffer
import edu.uci.ics.jung.visualization.decorators.AbstractVertexShapeTransformer
import java.awt.Shape
import scala.collection.JavaConversions._
import java.awt.Stroke
import org.apache.commons.collections15.functors.ConstantTransformer
import java.awt.Font
import java.lang.{ Integer => JInt }
import edu.uci.ics.jung.visualization.renderers.DefaultVertexLabelRenderer
object Methods extends SimpleSwingApplication {
  var file: String = _
  var alone = true
  val ui = new MethodUI({
    m =>
      selectedMethod = m.some
      label.text = m.name
  })
  override def startup(args: Array[String]) = {
    if (args.length >= 1)
      file = args(0)
    super.startup(args)
  }
  val label = new Label("Selected Method")
  val button = new Button("Hide") {
    reactions += {
      case ButtonClicked(e) => {
        selectedMethod.foreach {
          selected =>
            ui.applyHide(selected)
        }
      }
    }
  }

  var selectedMethod: Option[Method] = None
  var displayCU: CU = _
  def top = new MainFrame {
    title = "Graph View: Blue Nodes"

    val cu = {
      if (file != null)
        Serializer().unSerialize(FileUtil.fromFile(new File(file))).asInstanceOf[CU]
      else if (displayCU != null) displayCU
      else CU.empty
    }
    ui.setGraph(cu)
    contents = new BorderPanel() {
      add(Component.wrap(ui.viewer), Center)
      add(new FlowPanel(label, button), South)
    }
    override def closeOperation() {
      if (alone)
        sys.exit(0)
    }
  }

}


class MethodUI(vPicked: Method => Unit) extends JungUI[Method, Calling](vPicked) {
  import Transformers._
  override def newVisualizationViewer = new VisualizationViewer[Method, Calling](layout(createGraph(CU.empty)), new Dimension(800, 800))
  override def renderers(vv: VisualizationViewer[Method, Calling]) = {
    vv.getRenderContext.setVertexLabelTransformer({
      m: Method =>
        m.name
    })
    vv.getRenderContext.setVertexFillPaintTransformer(
      new Transformer[Method, Paint] {
        val in = Color.GREEN
        val out = Color.RED
        def list(color: Color) = List(color.getRed, color.getGreen, color.getBlue)
        def color(inout: Float) = {
          val r :: g :: b :: Nil = list(in).zip(list(out)).map {
            case (inp, outp) =>
              (inout * (inp - outp) + outp).toInt
          }
          new Color(r, g, b)
        }
        def alpha(color: Color, alpha: Int) = new Color(color.getRed, color.getGreen, color.getBlue, alpha)
        override def transform(m: Method): Paint = {

          val colorResult = {
            if (graph.degree(m) == 0) Color.BLUE
            else
              color(graph.inDegree(m).toFloat / (graph.outDegree(m) + graph.inDegree(m)))
          }

          if (!withPickedV(m))
            alpha(colorResult, 50)
          else
            colorResult
        }
      })

    val blackOrGrayM = {
      m: Method =>
        if (withPickedV(m)) Color.black else Color.lightGray
    }
    val blackOrGrayC = {
      c: Calling =>
        if (withPickedE(c)) Color.black else Color.lightGray
    }
    vv.getRenderContext.setEdgeDrawPaintTransformer(blackOrGrayC)
    vv.getRenderContext.setArrowFillPaintTransformer(blackOrGrayC)
    vv.getRenderContext.setArrowDrawPaintTransformer(blackOrGrayC)

    vv.getRenderContext.setVertexDrawPaintTransformer(blackOrGrayM)

    vv.getRenderContext.setVertexFontTransformer({
      m: Method =>
        if (withPickedV(m))
          new Font(Font.SANS_SERIF, Font.BOLD, 10)
        else
          new Font(Font.SANS_SERIF, Font.PLAIN, 9)
    })

    vv.getRenderContext.setVertexShapeTransformer(
      new AbstractVertexShapeTransformer[Method]() {
        setSizeTransformer({
          m: Method => new JInt(((m.size.toFloat / cu.maxMethodSize) * 50 + 5).toInt)
        })
        override def transform(m: Method) = factory.getEllipse(m)
      })

    vv
  }

  var cu: CU = _
  def setGraph(cu: CU): Unit = {
    this.cu = cu
    setGraph(createGraph(cu))
  }

  private def createGraph(cu: CU) = {
    val graph = new DirectedSparseGraph[Method, Calling]()
    cu.methods.foreach {
      graph.addVertex(_)
    }
    cu.callings.foreach {
      c =>
        graph.addEdge(c, c.from, c.to)
    }

    graph
  }
  override def layout(graph: Graph[Method, Calling]) = new FRLayout[Method, Calling](graph)

}

