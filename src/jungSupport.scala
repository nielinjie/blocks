package nielinjie.app.blocks
package ui

import edu.uci.ics.jung.visualization.control.PluggableGraphMouse
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.TranslatingGraphMousePlugin
import java.awt.Event
import edu.uci.ics.jung.visualization.control.ScalingGraphMousePlugin
import edu.uci.ics.jung.visualization.control.PickingGraphMousePlugin
import edu.uci.ics.jung.visualization.control.CrossoverScalingControl
import edu.uci.ics.jung.visualization.picking.ShapePickSupport
import java.awt.event.ItemListener
import java.awt.event.ItemEvent
import org.apache.commons.collections15.Predicate
import edu.uci.ics.jung.graph.util.Context
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.algorithms.layout.Layout
import scalaz._
import Scalaz._
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._
import org.apache.commons.collections15.Transformer

abstract class JungUI[V, E](vPickedFun: V => Unit) {
  def mouseControlls = {
    val graphMouse = new PluggableGraphMouse()
    graphMouse.add(new TranslatingGraphMousePlugin(Event.META_MASK))
    graphMouse.add(new ScalingGraphMousePlugin(new CrossoverScalingControl(), 0))
    graphMouse.add(new PickingGraphMousePlugin())
    graphMouse
  }
  def newVisualizationViewer: VisualizationViewer[V, E]
  def renderers(vv: VisualizationViewer[V, E])
  def graph = viewer.getGraphLayout.getGraph
  def picked = viewer.getPickedVertexState.getPicked
  lazy val viewer = {
    val vv = newVisualizationViewer
    renderers(vv)
    vv.setGraphMouse(mouseControlls)
    vv.setPickSupport(new ShapePickSupport(vv))
    vv.getPickedVertexState().addItemListener(new ItemListener {
      override def itemStateChanged(e: ItemEvent) = vPickedFun(e.getItem.asInstanceOf[V])
    })

    vv.getRenderContext().setVertexIncludePredicate(hiddenPredicate)
    vv
  }
  val hiddenPredicate = new Predicate[Context[Graph[V, E], V]] {
    val hidden = new ArrayBuffer[V]()
    override def evaluate(context: Context[Graph[V, E], V]) = {
      !hidden.contains(context.element)
    }
  }
  def withPickedV(m: V): Boolean = {
    val neighbors = picked.flatMap {
      graph.getNeighbors(_).toSet
    }
    picked.isEmpty || (neighbors.contains(m) || picked.contains(m))
  }

  def withPickedE(c: E): Boolean = {
    picked.isEmpty || picked.exists({ m: V =>
      graph.isDest(m, c) || graph.isSource(m, c)
    })
  }
  def applyHide(mehtodInfo: V) = {
    hiddenPredicate.hidden += mehtodInfo
    viewer.revalidate
    viewer.repaint()
  }
  def setGraph(graph: Graph[V, E]): Unit = {
    viewer.setGraphLayout(layout(graph))
    viewer.revalidate
    viewer.repaint()
  }
  def layout(graph: Graph[V, E]): Layout[V, E]
}

object Transformers {
  implicit def fun2Transform[A, B](fun: A => B): Transformer[A, B] = new Transformer[A, B] {
    override def transform(n: A): B = fun(n)
  }
  implicit def any2Transform[A, B](b: B): Transformer[A, B] = new Transformer[A, B] {
    override def transform(n: A): B = b
  }
}