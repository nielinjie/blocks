package nielinjie.app.blocks
package ui

import domain._
import edu.uci.ics.jung.visualization.VisualizationViewer
import java.awt.Dimension
import edu.uci.ics.jung.graph.DirectedSparseGraph
import edu.uci.ics.jung.algorithms.layout.FRLayout
import edu.uci.ics.jung.graph.Graph
import scala.swing.SimpleSwingApplication
import scala.swing.Label
import scala.swing.MainFrame
import nielinjie.util.io.Serializer
import nielinjie.util.io.FileUtil
import scala.swing.BorderPanel
import scala.swing.Component
import scala.swing.BorderPanel.Position._
import java.io.File
import scala.swing.FlowPanel
import org.eclipse.core.runtime.Platform
import edu.uci.ics.jung.algorithms.layout.SpringLayout
import edu.uci.ics.jung.algorithms.layout.SpringLayout2
import edu.uci.ics.jung.algorithms.layout.FRLayout2
import edu.uci.ics.jung.visualization.decorators.AbstractVertexShapeTransformer
import java.awt.geom.AffineTransform
import java.awt.Color
import scala.swing.ListView
import scalaz._
import Scalaz._
import nielinjie.util.ui.MigPanel
import nielinjie.util.ui.Mig
import scala.swing.ScrollPane
import scala.swing.SplitPane
import org.apache.commons.lang3.StringEscapeUtils
import blocks.Activator
import scala.swing.TextField
import scala.swing.EditorPane

object Flows extends SimpleSwingApplication {
  var file: String = _
  var alone = true
  val fillStatemes: Forwarable => Unit = {
    case b: Block => statementsLabel.text = BlockLabel.completeLabel(b)
  }
  val fillCalling: Forwarable => Unit = {
    case c: Callings => {
      callingList.listData = c.callings.map {
        bc =>
          bc.to |> {
            me: Method =>
              "%s.%s(%s)".format(me.typeName, me.name, me.argNames)
          }
      }
    }
  }
  val fillComment: Forwarable => Unit = {
    case c: WithComments =>{
//      Activator.getDefault.log(c.comments(blockedMethod).map(_.string).toString)
//      Activator.getDefault.log(StringEscapeUtils.escapeHtml4(c.comments(blockedMethod).map(_.string).toString))
      //memoLabel.text ="<html>%s</html>".format( c.comments(blockedMethod).map(co=>StringEscapeUtils.escapeHtml4(co.string)).mkString("<P>"))
      memoLabel.text = (c.comments(blockedMethod).map(co=>co.string).mkString("\n"))
    }
  }
  val fillUI: (Forwarable => Unit) = {
    f =>
      fillStatemes(f)
      fillComment(f)
      fillCalling(f)

  }

  val ui = new FlowUI(fillUI)

  val callingList = new ListView[String]

  override def startup(args: Array[String]) = {
    if (args.length >= 1)
      file = args(0)
    super.startup(args)
  }
  val memoLabel = new EditorPane//("Memo of selected block")
  memoLabel.editable=false
  val statementsLabel = new EditorPane
  statementsLabel.editable=false

  var blockedMethod: BlockedMethod = _
  def top = new MainFrame {
    title = "Block Flow"

    val flow = {
      if (file != null)
        Serializer().unSerialize(FileUtil.fromFile(new File(file))).asInstanceOf[Flow[Block, Forward]]
      else if (blockedMethod != null) blockedMethod.blockFlow
      else BlockFlow.empty
    }
    ui.setGraph(flow)
    import Mig._
    contents = new MigPanel(fill, fill(900) | fill(300), fill(800)) {
      add(Component.wrap(ui.viewer), "")
      add((new MigPanel(fill, fill, fill) {
        add(new ScrollPane(memoLabel), wrap)
        add(new ScrollPane(statementsLabel), wrap)
        add(new ScrollPane(callingList), "")
      }), "")

    }
    override def closeOperation() {
      if (alone)
        sys.exit(0)
    }
  }

}
class FlowUI(vPicked: Forwarable => Unit) extends JungUI[Forwarable, Forward](vPicked) {
  override def newVisualizationViewer = new VisualizationViewer[Forwarable, Forward](layout(createGraph(BlockFlow.empty)), new Dimension(800, 800))
  var blockFlow: Flow[Block, Forward] = _
  def setGraph(blockFlow: Flow[Block, Forward]): Unit = {
    this.blockFlow = blockFlow
    setGraph(createGraph(blockFlow))
  }

  private def createGraph(blockFlow: Flow[Block, Forward]) = {
    val graph = new DirectedSparseGraph[Forwarable, Forward]()
    blockFlow.vertexes.foreach {
      graph.addVertex(_)
    }
    blockFlow.edges.foreach {
      c =>
        graph.addEdge(c, c.from, c.to)
    }

    graph
  }
  override def layout(graph: Graph[Forwarable, Forward]) = new FRLayout[Forwarable, Forward](graph)
  override def renderers(vv: VisualizationViewer[Forwarable, Forward]) = {
    import Transformers._
    vv.getRenderContext.setVertexLabelTransformer { f: Forwarable =>
      (f match {
        case b: Block => BlockLabel.label(b)
        case a => a.toString
      }).toString
    }
    vv.getRenderContext.setVertexShapeTransformer(
      new AbstractVertexShapeTransformer[Forwarable]() {
        val transfer = AffineTransform.getRotateInstance(scala.math.Pi / 4)
        setSizeTransformer({
          n: Forwarable =>
            new java.lang.Integer(n match {
              case c: Callings => (10 + c.callings.size * 5).min(50)
              case _ => 10
            })
        })

        override def transform(node: Forwarable) = node match {
          case p: PlainBlock => factory.getRoundRectangle(p)
          case l: LoopBlock => factory.getEllipse(l)
          case i: IfBlock => transfer.createTransformedShape(factory.getRectangle(i))
          case _ => factory.getEllipse(node)
        }
      })
    vv.getRenderContext.setVertexFillPaintTransformer({
      n: Forwarable =>
        
        if (graph.getOutEdges(n).isEmpty || (n.isInstanceOf[FromStatements] && n.asInstanceOf[FromStatements].exitPoint))
          Color.green
        else if (graph.getInEdges(n).isEmpty)
          Color.red
        else
          new Color(127, 127, 0)
    })

  }
}
object BlockLabel {
  def rangeString(range:Range)="%d-%d".format(range.start,range.end)
  
  def label(block: Block): String = {
    "<html>%s</html>".format(block match {
      case p: PlainBlock => p.statements |> {
        statms =>
          (if (statms.size > 4)
            statms.take(2) :+ "<...%d more plain statement(s)...>".format(statms.size) :+ statms.takeRight(1)
          else
            statms).map(x => StringEscapeUtils.escapeHtml4(x.toString)).mkString("<p>")

      }
      case l: LoopBlock => "loop(%s)".format(StringEscapeUtils.escapeHtml4(l.expression.toString))
      case i: IfBlock => "if(%s)".format(StringEscapeUtils.escapeHtml4(i.expression.toString))
    })
  }
  def completeLabel(block: Block): String = {
    block match {
      case p: PlainBlock => p.statements.map(x => StringEscapeUtils.escapeHtml4(x.toString)).mkString("\n")
      case l: LoopBlock => "loop(%s)".format(StringEscapeUtils.escapeHtml4(l.expression.toString))
      case i: IfBlock => "if(%s)".format(StringEscapeUtils.escapeHtml4(i.expression.toString))
    }
  }
}