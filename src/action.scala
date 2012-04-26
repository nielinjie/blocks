package nielinjie.app.blocks
package ui

import org.eclipse.jface.action.IAction
import org.eclipse.jface.text.ITextSelection
import org.eclipse.ui.IEditorActionDelegate
import domain._
import org.eclipse.ui.PlatformUI
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.AST
import java.io.File
import org.eclipse.jface.text.TextSelection
import org.eclipse.jdt.core.IMethod
import org.eclipse.jface.viewers.ISelection
import org.eclipse.swt.widgets.Shell
import org.eclipse.ui.texteditor.ITextEditor
import org.eclipse.jface.text.IDocument
import org.eclipse.core.resources.IFile
import org.eclipse.jdt.core.ICompilationUnit
import org.eclipse.ui.IEditorPart
import org.eclipse.jdt.ui.JavaUI
import org.eclipse.ui.IFileEditorInput

class MehtodMapDisplayDelegate extends IEditorActionDelegate with FindContext {

  def run(action: IAction): Unit = {
    val parser = new Parser(compilationUnit)
    Methods.displayCU = CU.fromAST(parser.methods, parser.callings)
    Methods.alone = false
    Methods.main(Array[String]())
  }
}

class MehtodMapFileSaveDelegate extends IEditorActionDelegate with FindContext {

  def run(action: IAction): Unit = {
    val parser = new Parser(compilationUnit)
    CU.write(new File(file.getRawLocation.makeAbsolute.toString + ".blocks"), CU.fromAST(parser.methods, parser.callings))
  }

}

class BlockFlowFileSaveDelegate extends IEditorActionDelegate with FindContext {
  def run(action: IAction): Unit = {
    val position = targetEditor.getSelectionProvider.getSelection.asInstanceOf[TextSelection].getOffset
    val parser = new Parser(compilationUnit)
    parser.methodAt(position).foreach({
      method =>
        val bf = BlockedMethod.fromAST(method, parser.ecu,compilationUnit.getSource)
        BlockedMethod.write(new File("%s_%s.flow".format(file.getRawLocation.makeAbsolute.toString, bf.method.name)), bf)
    })
  }
}

class BlockFlowDisplayDelegate extends IEditorActionDelegate with FindContext {
  def run(action: IAction): Unit = {
    val position = targetEditor.getSelectionProvider.getSelection.asInstanceOf[TextSelection].getOffset
    val parser = new Parser(compilationUnit)
    parser.methodAt(position).foreach({
      method =>
        val bf = BlockedMethod.fromAST(method, parser.ecu,compilationUnit.getSource)
        Flows.blockedMethod = bf
        Flows.alone = false
        Flows.main(Array[String]())
    })
  }
}

trait FindContext {
  self: IEditorActionDelegate =>
  var selection: ISelection = _
  var shell: Shell = _
  var targetEditor: ITextEditor = _
  var document: IDocument = _
  var file: IFile = _
  var compilationUnit: ICompilationUnit = _
  def selectionChanged(action: IAction, selection: ISelection): Unit = {
    this.selection = selection
  }
  def setActiveEditor(action: IAction, editorPart: IEditorPart): Unit = {

    targetEditor = editorPart.asInstanceOf[ITextEditor]
    shell = editorPart.getSite().getWorkbenchWindow().getShell()
    document = targetEditor.getDocumentProvider.getDocument(targetEditor.getEditorInput())
    compilationUnit = JavaUI.getEditorInputJavaElement(targetEditor.getEditorInput()).asInstanceOf[ICompilationUnit]
    file = try { targetEditor.asInstanceOf[ITextEditor].getEditorInput().asInstanceOf[IFileEditorInput].getFile } catch { case e: Exception => null }

  }
}
