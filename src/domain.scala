package nielinjie.app.blocks
package domain

import org.eclipse.jdt.core.dom.IMethodBinding
import java.util.UUID
import scalaz._
import Scalaz._
import nielinjie.util.io._
import java.io.File

import org.eclipse.jdt.core.dom.MethodBinding

case class CU(methods: List[Method], callings: List[Calling]) {
  def maxMethodSize = methods.map(_.size).max
}
object CU {
  def empty = CU(List.empty[Method], List.empty[Calling])
  def toMethod(method: MethodAST): Method = Method(method.binding.getDeclaringClass.getName, method.binding.getName, UUID.randomUUID,
    method.binding.getParameterTypes.map({
      t =>
        t.getName
    }).mkString(","), method.size)
  def fromAST(methods: List[MethodAST], callings: List[CallingAST]): CU = {

    val methodPairs = methods.zip(methods.map {
      method: MethodAST =>
        toMethod(method)
    })
    CU(methodPairs.map(_._2),
      callings.map {
        calling =>
          (methodPairs.find(_._1.binding == calling.from) |@| methodPairs.find(_._1.binding == calling.to))
            .apply((mp, mp2) => Calling(mp._2, mp2._2))
      }.flatten)
  }
  def write(file: File, cu: CU) = {
    val infoxml = Serializer().serialize(cu)
    FileUtil.toFile(infoxml, file)
  }
}

case class Method(typeName: String, name: String, id: UUID, argNames: String, size: Int)
case class Calling(from: Method, to: Method)
