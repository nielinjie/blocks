package nielinjie.app.blocks
package domain

import scalaz._
import Scalaz._
import blocks.Activator

abstract class Flow[V, E](val start: Option[V], val ends: List[V], val vertexes: List[V], val edges: List[E]) {
  require(start.forall(vertexes.contains(_)))
  require(ends.forall(vertexes.contains(_)))
  if(start.isEmpty) {
    require(ends.isEmpty)
    require(vertexes.isEmpty)
    require(edges.isEmpty)
  }
  def connect(flow: Flow[V, E]) = {
    if (start.isEmpty)
      flow
    else {
      val newForwards: List[E] = for {
        end <- this.ends
        start <- flow.start
      } yield (newEdge(end, start, ""))
      newFlow(start, flow.ends, this.vertexes ::: flow.vertexes, this.edges ::: flow.edges ::: newForwards)
    }
  }
  def connectOnVertex(vertex: V, flow: Flow[V, E], eName: String) = {
    require(this.vertexes.contains(vertex))
    val newForwards = flow.start.map(newEdge(vertex, _, eName)).toList
    Activator.getDefault.log("flow.start - %s newForwards - %s".format(flow.start, newForwards.toString))
    //throw new RuntimeException(newForwards.toString)
    newFlow(start, (this.ends.filterNot(_ == vertex)) ::: flow.ends, this.vertexes ::: flow.vertexes, this.edges ::: flow.edges ::: newForwards)
  }
  def connetVertexOnVertex(from: V, to: V, eName: String) = {
    require(this.vertexes.contains(from))
    require(this.vertexes.contains(to))
    newFlow(this.start, this.ends, this.vertexes, newEdge(from, to, eName) :: this.edges)
  }
  def newEdge(from: V, to: V, name: String): E
  def newFlow(start: Option[V], ends: List[V], vertexes: List[V], edges: List[E]): Flow[V, E]
}

object FlowMain extends App {
  class IntFlow(start: Option[Int], ends: List[Int], vertexes: List[Int], edges: List[SimpleE]) extends Flow[Int, SimpleE](start, ends, vertexes, edges) {
    def newEdge(from: Int, to: Int, name: String): SimpleE = SimpleE(from, to, name)
    def newFlow(start: Option[Int], ends: List[Int], vertexes: List[Int], edges: List[SimpleE]): Flow[Int, SimpleE] = new IntFlow(start, ends, vertexes, edges)
    override def toString() = {
      "start -%s \n ends -%s \n vertexes -%s \n edges -%s".format(start, ends, vertexes, edges)
    }
  }
  case class SimpleE(val from: Int, val to: Int, val name: String)
  val one = new IntFlow(1.some, List(1), List(1), List.empty)
  val two = new IntFlow(2.some, List(2), List(2), List.empty)
  val three = new IntFlow(3.some, List(3), List(3), List.empty)
  val onetwothree = one.connectOnVertex(1, two, "1 to 2").connectOnVertex(1, three, "1 to 3")
  val four = new IntFlow(4.some, List(4), List(4), List.empty)
  println(onetwothree.connect(four))
  //println(onetwo.connetVertexOnVertex(1, 2, "vonv"))

  //  val one = new IntFlow(1.some,List(1),List(1),List.empty)
}