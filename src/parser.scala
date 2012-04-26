package nielinjie.app.blocks
package domain

import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.ICompilationUnit
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.SourceRange
import org.eclipse.jdt.core.ISourceRange
import org.eclipse.jdt.core.dom.ASTVisitor
import org.eclipse.jdt.core.dom.MethodDeclaration
import scala.collection.mutable.ArrayBuffer
import org.eclipse.jdt.core.dom.MethodInvocation
import org.eclipse.jdt.core.dom.IMethodBinding
import org.eclipse.jdt.core.IMethod
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scalaz._
import Scalaz._
import nielinjie.util.io.FileUtil
import org.eclipse.jdt.core.dom.Statement
import org.eclipse.jdt.core.dom.{ Block => EBlock }
import org.eclipse.jdt.core.dom.{ Comment => EComment, CompilationUnit => ECU }
import org.eclipse.jdt.core.dom.ASTNode

class Parser(compilationUnit: ICompilationUnit) {
  lazy val ast = {
    val parser: ASTParser = ASTParser.newParser(AST.JLS3)
    parser.setResolveBindings(true)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setSource(compilationUnit)
    parser.createAST(null)
  }
  val allMethods: List[(MethodDeclaration, IMethodBinding)] = {
    val visitor = new MethodDeclarationVisitor
    ast.accept(visitor)

    visitor.methods.map {
      meth =>
        (meth, meth.resolveBinding)
    }
      .toList
  }
  def methodAt(position: Int) = methods.find {
    meth =>
      meth.start <= position && meth.start + meth.size >= position
  }
  lazy val methods: List[MethodAST] = allMethods.map({
    meth =>
      MethodAST(meth._2, meth._1.getBody, meth._1.getLength, meth._1.getStartPosition)
  })
  lazy val callings: List[CallingAST] = allMethods.map({
    meth =>
      val visitor = new MethodInvocationVisitor
      Option(meth._1.getBody).foreach(_.accept(visitor))
      visitor.callings.map({
        invocation =>
          CallingAST(meth._2, invocation.resolveMethodBinding)
      })
  }).toList.flatten
  lazy val cu = CU.fromAST(methods, callings)
  lazy val ecu:ECU = {
    var cur: ECU = null
    ast.accept(new ASTVisitor {
      override def visit(cu: ECU): Boolean = {
        cur = cu
        false
      }
    })
    cur
  }
}
object Parser {

  def callingsFromNode(node: ASTNode): List[IMethodBinding] = {
    val v = new MethodInvocationVisitor()
    node.accept(v)
    v.callings.map(_.resolveMethodBinding).toList
  }

}

class MethodInvocationVisitor extends ASTVisitor {
  val callings = new ArrayBuffer[MethodInvocation]()
  override def visit(methodInvocation: MethodInvocation) = {
    callings += methodInvocation
    super.visit(methodInvocation)
  }
}
class MethodDeclarationVisitor extends ASTVisitor {
  val methods = new ArrayBuffer[MethodDeclaration]()
  override def visit(methodDeclaration: MethodDeclaration) = {
    methods += methodDeclaration
    super.visit(methodDeclaration)
  }
}
case class MethodAST(binding: IMethodBinding, body: EBlock, size: Int, start: Int)
case class CallingAST(from: IMethodBinding, to: IMethodBinding)

