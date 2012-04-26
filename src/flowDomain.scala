package nielinjie.app.blocks
package domain

import org.eclipse.jdt.core.dom.Statement
import org.eclipse.jdt.core.dom.ForStatement
import org.eclipse.jdt.core.dom.WhileStatement
import org.eclipse.jdt.core.dom.DoStatement
import org.eclipse.jdt.core.dom.IfStatement
import org.eclipse.jdt.core.dom.TryStatement
import org.eclipse.jdt.core.dom.{ Block => EBlock, CompilationUnit => ECU, Comment => EComment }
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scalaz._
import Scalaz._
import blocks.Activator
import java.util.UUID
import scala.util.control.Exception._
import org.eclipse.jdt.core.dom.SynchronizedStatement
import java.io.File
import nielinjie.util.io.Serializer
import nielinjie.util.io.FileUtil
import org.eclipse.jdt.core.dom.MethodInvocation
import org.eclipse.jdt.core.dom.Expression
import org.eclipse.jdt.core.dom.LineComment
import org.eclipse.jdt.core.dom.ReturnStatement
import javax.swing.text.html.BlockView

case class Forward(from: Forwarable, to: Forwarable, key: String)
case class BlockCalling(from: Block, to: Method)()
case class Comment(string: String)

class BlockFlow(start: Option[Block], ends: List[Block], blocks: List[Block], forwards: List[Forward]) extends Flow[Block, Forward](start, ends, blocks, forwards) {
  override def newEdge(from: Block, to: Block, name: String) = Forward(from, to, name)
  override def newFlow(start: Option[Block], ends: List[Block], vertexes: List[Block], edges: List[Forward]) = new BlockFlow(start, ends, vertexes, edges)
  def trimEmpty = {
    def findIn(node: EmptyBlock) = {
      this.edges.filter(_.to == node).ensuring(_.size == 1).head
    }
    def findOut(node: EmptyBlock) = {
      this.edges.filter(_.from == node).ensuring(_.size <= 1).headOption
    }
    val emptys = this.blocks.filter(_.isInstanceOf[EmptyBlock])
    val edgs: List[(Forward, Option[Forward], Block)] = (emptys.map {
      case node: EmptyBlock =>
        allCatch((findIn(node), findOut(node), node).some).orElse(None)
      case _ => None
    }).flatten
    edgs.foldLeft(this) {
      case (flow, (in, Some(out), node)) =>
        new BlockFlow(flow.start.filterNot(_ == node), flow.ends.filterNot(_ == node), flow.vertexes.filterNot(_ == node), flow.edges.filterNot(e => e == in || e == out) :+ (new Forward(in.from, out.to, in.key + out.key)))
      case (flow, (in, None, node)) =>
        new BlockFlow(flow.start.filterNot(_ == node), flow.ends.filterNot(_ == node), flow.vertexes.filterNot(_ == node), flow.edges.filterNot(e => e == in))
    }
  }

  //def calling: List[BlockCalling] = List.empty
}
case class Exitor(name: String) extends Forwarable
object BlockFlow {
  val empty: Flow[Block, Forward] = {
    new BlockFlow(None, List(), List(), List())
  }
  def fromSimpleBlock(block: Block): Flow[Block, Forward] = {
    new BlockFlow(block.some, List(block), List(block), List())
  }
}
trait Block extends FromStatements with Forwarable {
  def flowView: Flow[Block, Forward]
}
trait FromStatements extends WithComments {
  val statements: List[Statement]
  //lazy val begin: Option[Int] = Option(statements).map(_.headOption.map(_.getStartPosition)).join
  lazy val sourceRange: Option[Range] = (Option(statements).map {
    stats =>
      for {
        start <- stats.headOption.map(_.getStartPosition)
        end <- stats.lastOption.map {
          lastState =>
            lastState.getStartPosition + lastState.getLength
        }
      } yield (Range(start, end))
  }).join
  lazy val exitPoint = statements.find(_.isInstanceOf[ReturnStatement]).isDefined
}
trait WithComments {
  self: FromStatements =>
  def comments(implicit blockedMethod: BlockedMethod): List[Comment] = {
    def source(begin: Int, length: Int) = blockedMethod.source.substring(begin, begin + length)
    if (statements.isEmpty) List.empty
    else {
      val commentList: List[EComment] = blockedMethod.cu.getCommentList.asInstanceOf[java.util.List[EComment]].toList
      val firstCommentIndex = statements.map({
        state =>
          blockedMethod.cu.firstLeadingCommentIndex(state)
      }).find(_ != -1)
      val firstBeing = statements.map(
        blockedMethod.cu.getExtendedStartPosition(_)).min
      val lastEnd = statements.map(
        state =>
          state.getStartPosition + state.getLength).max
      Activator.getDefault.log(firstBeing.toString)
      Activator.getDefault.log(lastEnd.toString)
      commentList.filter(comment => comment.getStartPosition |> (p => firstBeing <= p && p < lastEnd)).map {
        com =>
          Comment(source(com.getStartPosition, com.getLength))
      }
    }
  }
}
trait Callings {
  def callings: List[BlockCalling]
}

trait Forwarable

class EmptyBlock() extends Block {
  val id = UUID.randomUUID
  val comment = None
  def flowView = BlockFlow.fromSimpleBlock(this)
  val statements = List.empty
}
case class PlainBlock(statements: List[Statement]) extends Block with Callings {
  def flowView = BlockFlow.fromSimpleBlock(this)
  lazy val callings: List[BlockCalling] = statements.map {
    statement =>
      Parser.callingsFromNode(statement).map {
        binding =>
          Method(binding.getDeclaringClass.getName, binding.getName, UUID.randomUUID,
            binding.getParameterTypes.map({
              t =>
                t.getName
            }).mkString(","), 0)
      }.map {
        method: Method =>
          BlockCalling(this, method)
      }
  }.flatten

}
case class IfBlock(trueB: Flow[Block, Forward], falseB: Flow[Block, Forward], statements: List[Statement], expression: Expression) extends Block with Callings {
  def flowView = {
    val if_ = BlockFlow.fromSimpleBlock(this)
    val true_ = if_.connectOnVertex(this, trueB, "true")
    val false_ = true_.connectOnVertex(this, falseB, "false")
    false_ |>{
      f=>
    	new BlockFlow(f.start,f.ends.filterNot(_.exitPoint),f.vertexes,f.edges)
    }
  }
  lazy val callings: List[BlockCalling] =
    Parser.callingsFromNode(expression).map {
      binding =>
        Method(binding.getDeclaringClass.getName, binding.getName, UUID.randomUUID,
          binding.getParameterTypes.map({
            t =>
              t.getName
          }).mkString(","), 0)
    }.map {
      method: Method =>
        BlockCalling(this, method)
    }

}
case class LoopBlock(body: Flow[Block, Forward], statements: List[Statement], expression: Expression) extends Block with Callings {
  def flowView = {
    val bodyFlow = body
    val connectedToLoopNode = bodyFlow.ends.foldLeft(BlockFlow.fromSimpleBlock(this).connectOnVertex(this, bodyFlow, "loopBody")) {
      (blockFlow, endBlock) =>
        blockFlow.connetVertexOnVertex(endBlock, this, "loop")
    }
    new BlockFlow(connectedToLoopNode.start, List(this), connectedToLoopNode.vertexes, connectedToLoopNode.edges)
  }
  lazy val callings: List[BlockCalling] =
    Parser.callingsFromNode(expression).map {
      binding =>
        Method(binding.getDeclaringClass.getName, binding.getName, UUID.randomUUID,
          binding.getParameterTypes.map({
            t =>
              t.getName
          }).mkString(","), 0)
    }.map {
      method: Method =>
        BlockCalling(this, method)
    }
}
case class TryBlock(tr: Flow[Block, Forward], catchs: List[Block], statements: List[Statement]) extends Block {
  def flowView = {
    //TODO what's view for try/catch branch?
    tr
  }
}
case class SynchronizedBlock(body: Flow[Block, Forward], statements: List[Statement]) extends Block {
  def flowView = {
    body
  }
}

class BlockedMethod(val method: Method, val blockFlow: BlockFlow, val cu: ECU, val source: String) {

}
object BlockedMethod {

  def fromAST(methodAST: MethodAST, cu: ECU, source: String): BlockedMethod = {
    val method = CU.toMethod(methodAST)
    new BlockedMethod(method, blockFlowFromAST(methodAST.body).asInstanceOf[BlockFlow].trimEmpty, cu, source)
  }
  def blockFlowFromAST(statement: Statement): Flow[Block, Forward] = {
    val statements = if (statement == null)
      List.empty
    else if (statement.isInstanceOf[EBlock]) {
      statement.asInstanceOf[EBlock].statements.asInstanceOf[java.util.List[Statement]].toList
    } else List(statement)
    if (statements.empty) {
      BlockFlow.fromSimpleBlock(new EmptyBlock())
    } else {
      val blocks = splitByNormal(statements).map {
        splitedStatment: List[Statement] =>
          astToBlock(splitedStatment)
      }
      Activator.getDefault.log(blocks.toString)
      val f = blocks.foldLeft(BlockFlow.empty) {
        (flow, blockF) =>
          val ff = blockF.flowView
          flow.connect(ff)
      }
      Activator.getDefault.log(f.edges.toString)
      f
    }
  }
  def astToBlock(splitedStatment: List[Statement]): Block = {
    require(!splitedStatment.isEmpty)
    if (splitedStatment.size != 1) {
      PlainBlock(splitedStatment)
    } else {
      val state = splitedStatment.head
      state match {
        case i: IfStatement => IfBlock(blockFlowFromAST(i.getThenStatement), blockFlowFromAST(i.getElseStatement), splitedStatment, i.getExpression)
        case d: DoStatement => LoopBlock(blockFlowFromAST(d.getBody), splitedStatment, d.getExpression)
        case w: WhileStatement => LoopBlock(blockFlowFromAST(w.getBody), splitedStatment, w.getExpression)
        case f: ForStatement => LoopBlock(blockFlowFromAST(f.getBody), splitedStatment, f.getExpression)
        case t: TryStatement => TryBlock(blockFlowFromAST(t.getBody), List.empty, splitedStatment)
        case s: SynchronizedStatement => SynchronizedBlock(blockFlowFromAST(s.getBody), splitedStatment)
        case _ => PlainBlock(splitedStatment)
      }
    }
  }
  def splitByNormal(statements: List[Statement]): List[List[Statement]] = {
    val splitingPair = (statements.foldLeft((List.empty[List[Statement]], List.empty[Statement])) {
      case ((splited, temp), state) =>
        if (isNormal(state)) {
          (splited, temp :+ state)
        } else {
          (splited :+ temp :+ List(state), List.empty[Statement])
        }
    })
    (splitingPair._1 :+ splitingPair._2).filterNot(_.isEmpty)
  }
  def isNormal(statement: Statement): Boolean = {
    !(statement.isInstanceOf[ForStatement]
      || statement.isInstanceOf[WhileStatement]
      || statement.isInstanceOf[IfStatement]
      || statement.isInstanceOf[DoStatement]
      || statement.isInstanceOf[TryStatement]
      || statement.isInstanceOf[SynchronizedStatement])
  }

  def write(file: File, bm: BlockedMethod) = {
    val infoxml = Serializer().serialize(bm)
    FileUtil.toFile(infoxml, file)
  }
}
//object FlowDomain extends App {
//  val iff =
//    IfBlock(None,
//      PlainBlock(Comment("true").some, List.empty).flowView,
//      PlainBlock(Comment("false").some, List.empty).flowView,
//      List.empty)
//  val last = PlainBlock(Comment("last").some, List.empty)
//  val v = iff.flowView.connect(last.flowView)
//}